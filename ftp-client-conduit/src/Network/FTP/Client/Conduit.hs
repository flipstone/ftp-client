{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Network.FTP.Client.Conduit
Description : Transfer files over FTP and FTPS with Conduit
Copyright   : Megan Robinson 2018-2019, Flipstone Technology Partners 2024-2026
License     : Public Domain
Stability   : experimental
Portability : POSIX
-}
module Network.FTP.Client.Conduit
  ( -- * Data commands
    nlst
  , retr
  , list
  , stor
  , mlsd
  ) where

-- MonadResource appears in this module's exported signatures. Conduit
-- re-exports it, so importing it from resourcet by name is what keeps the
-- resourcet dependency genuinely used rather than implicit.

import Conduit ((.|))
import qualified Conduit
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MIO
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Network.FTP.Client
  ( FTPCommand (..)
  , PortActivity (..)
  , RTypeCode (..)
  , Security (..)
  , createSendDataCommand
  , createTLSSendDataCommand
  , drainPendingCompletion
  , getResponse
  , newPendingCompletion
  , parseMlsxLine
  , requireTLSContext
  , sIOHandleImpl
  , sendCommandS
  , tlsHandleImpl
  )
import qualified System.IO as SIO

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Network.Connection as Connection
import qualified Network.FTP.Client as FTP

debugging :: Bool
debugging = False

debugPrint :: (Show a, MIO.MonadIO m) => a -> m ()
debugPrint s =
  if debugging
    then MIO.liftIO $ print s
    else return ()

debugResponse :: (Show a, MIO.MonadIO m) => a -> m ()
debugResponse s = debugPrint $ "Recieved: " <> (show s)

getAllLineRespC ::
  forall i m.
  MIO.MonadIO m =>
  FTP.Handle ->
  Conduit.ConduitT i ByteString m ()
getAllLineRespC h =
  let
    loop :: Conduit.ConduitT i ByteString m ()
    loop = do
      -- End of input is signalled by getLineRespMaybe returning Nothing. A
      -- blank line is reply content, not a terminator: treating it as one
      -- silently dropped the rest of a listing and the caller still saw the
      -- normal 226. Any other IO failure propagates rather than masquerading
      -- as a complete transfer.
      mLine <- MIO.liftIO $ FTP.getLineRespMaybe h
      case mLine of
        Nothing -> return ()
        Just line -> do
          Monad.unless (B.null line) $ Conduit.yield line
          loop
  in
    loop

sendAllLineC ::
  forall o m.
  MIO.MonadIO m =>
  FTP.Handle ->
  Conduit.ConduitT ByteString o m ()
sendAllLineC h =
  let
    loop :: ByteString -> Conduit.ConduitT ByteString o m ()
    loop carry = do
      mx <- Conduit.await
      case mx of
        Nothing ->
          -- Trailing bytes with no final newline: send them as-is rather than
          -- inventing a terminator the input did not have.
          Monad.unless (B.null carry) . MIO.liftIO $
            FTP.send h (FTP.toNetworkAscii carry)
        Just x -> do
          let
            -- Hold back whatever follows the last newline; the rest of that
            -- line may be in the next chunk.
            (complete, rest) = C.breakEnd (== '\n') (carry <> x)
          Monad.unless (B.null complete) . MIO.liftIO $
            FTP.send h (FTP.toNetworkAscii complete)
          loop rest
  in
    loop ""

sourceDataCommandSecurity ::
  MonadResource m =>
  FTP.Handle ->
  PortActivity ->
  RTypeCode ->
  FTPCommand ->
  (FTP.Handle -> Conduit.ConduitM i o m r) ->
  Conduit.ConduitM i o m r
sourceDataCommandSecurity h =
  case FTP.security h of
    Clear -> sourceDataCommand h
    TLS _ -> sourceTLSDataCommand h

sourceDataCommand ::
  MonadResource m =>
  FTP.Handle ->
  PortActivity ->
  RTypeCode ->
  FTPCommand ->
  (FTP.Handle -> Conduit.ConduitM i o m r) ->
  Conduit.ConduitM i o m r
sourceDataCommand ch pa code cmd f = do
  _ <- sendCommandS ch $ RType code
  -- Reading the completion reply is part of releasing the data connection, not
  -- a later statement. Downstream terminating early -- takeC, headC, any short
  -- circuit -- abandons this pipeline, and a reply left unread becomes the
  -- answer to the next command for the rest of the session.
  -- bracketP runs no release action when acquisition itself fails, and part of
  -- acquisition happens after the server has accepted the transfer. Draining
  -- has to be attached to the acquire for those, or the completion reply is
  -- left to become the answer to the next command.
  Conduit.bracketP
    ( do
        pending <- newPendingCompletion
        createSendDataCommand ch pa pending cmd
          `Exception.onException` drainPendingCompletion ch pending
    )
    ( \dataHandle -> do
        SIO.hClose dataHandle
        getResponse ch >>= debugResponse
    )
    (f . sIOHandleImpl)

sourceTLSDataCommand ::
  MonadResource m =>
  FTP.Handle ->
  PortActivity ->
  RTypeCode ->
  FTPCommand ->
  (FTP.Handle -> Conduit.ConduitM i o m r) ->
  Conduit.ConduitM i o m r
sourceTLSDataCommand ch pa code cmd f = do
  tlsContext <- requireTLSContext ch
  _ <- sendCommandS ch $ RType code
  -- As above, and this is the path where it bites hardest: the data channel
  -- handshake runs after the preliminary reply, so a rejected certificate
  -- closes the data socket with the completion reply still queued.
  Conduit.bracketP
    ( do
        pending <- newPendingCompletion
        createTLSSendDataCommand ch pa pending cmd
          `Exception.onException` drainPendingCompletion ch pending
    )
    ( \conn -> do
        Connection.connectionClose conn
        getResponse ch >>= debugResponse
    )
    (f . tlsHandleImpl tlsContext)

sourceFTPHandle ::
  forall i m.
  MIO.MonadIO m =>
  FTP.Handle ->
  Conduit.ConduitT i ByteString m ()
sourceFTPHandle h =
  let
    loop :: Conduit.ConduitT i ByteString m ()
    loop = do
      bs <- MIO.liftIO $ FTP.recv h defaultChunkSize
      if B.null bs
        then return ()
        else do
          Conduit.yield bs
          loop
  in
    loop

sinkFTPHandle ::
  forall o m.
  MIO.MonadIO m =>
  FTP.Handle ->
  Conduit.ConduitT ByteString o m ()
sinkFTPHandle h =
  let
    loop :: Conduit.ConduitT ByteString o m ()
    loop = do
      mbs <- Conduit.await
      case mbs of
        Nothing -> return ()
        Just bs -> do
          MIO.liftIO $ FTP.send h bs
          loop
  in
    loop

sendType ::
  MonadResource m =>
  RTypeCode ->
  FTP.Handle ->
  Conduit.ConduitT ByteString o m ()
sendType TA h = sendAllLineC h
sendType TI h = sinkFTPHandle h

nlst :: MonadResource m => FTP.Handle -> [String] -> Conduit.ConduitT i ByteString m ()
nlst ch args =
  sourceDataCommandSecurity ch Passive TA (Nlst args) getAllLineRespC

retr :: MonadResource m => FTP.Handle -> String -> Conduit.ConduitT i ByteString m ()
retr ch path =
  sourceDataCommandSecurity ch Passive TI (Retr path) sourceFTPHandle

list :: MonadResource m => FTP.Handle -> [String] -> Conduit.ConduitT i ByteString m ()
list ch args =
  sourceDataCommandSecurity ch Passive TA (List args) getAllLineRespC

stor ::
  MonadResource m =>
  FTP.Handle ->
  String ->
  RTypeCode ->
  Conduit.ConduitT ByteString o m ()
stor ch loc rtype =
  sourceDataCommandSecurity ch Passive rtype (Stor loc) $ sendType rtype

mlsd ::
  MonadResource m =>
  FTP.Handle ->
  String ->
  Conduit.ConduitT i FTP.MlsxResponse m ()
mlsd ch dir =
  sourceDataCommandSecurity ch Passive TA (Mlsd dir) getAllLineRespC
    .| Conduit.mapC parseMlsxLine
