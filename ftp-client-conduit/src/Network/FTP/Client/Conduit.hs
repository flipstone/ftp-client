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
  , getResponse
  , parseMlsxLine
  , sIOHandleImpl
  , sendCommandS
  , tlsHandleImpl
  )
import qualified System.IO as SIO

import qualified Control.Monad.Catch as M
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
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
      line <-
        MIO.liftIO $
          FTP.getLineResp h `M.catchIOError` const (return "")
      if B.null line
        then return ()
        else do
          Conduit.yield line
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
    loop :: Conduit.ConduitT ByteString o m ()
    loop = do
      mx <- Conduit.await
      case mx of
        Nothing -> return ()
        Just x -> do
          MIO.liftIO $ FTP.sendLine h x
          loop
  in
    loop

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
    TLS -> sourceTLSDataCommand h

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
  x <-
    Conduit.bracketP
      (createSendDataCommand ch pa cmd)
      (MIO.liftIO . SIO.hClose)
      (f . sIOHandleImpl)
  resp <- getResponse ch
  debugResponse resp
  return x

sourceTLSDataCommand ::
  MonadResource m =>
  FTP.Handle ->
  PortActivity ->
  RTypeCode ->
  FTPCommand ->
  (FTP.Handle -> Conduit.ConduitM i o m r) ->
  Conduit.ConduitM i o m r
sourceTLSDataCommand ch pa code cmd f = do
  _ <- sendCommandS ch $ RType code
  x <-
    Conduit.bracketP
      (createTLSSendDataCommand ch pa cmd)
      (MIO.liftIO . Connection.connectionClose)
      (f . tlsHandleImpl)
  resp <- getResponse ch
  debugResponse resp
  return x

sourceFTPHandle ::
  forall i m.
  MIO.MonadIO m =>
  FTP.Handle ->
  Conduit.ConduitT i ByteString m ()
sourceFTPHandle h =
  let
    loop :: Conduit.ConduitT i ByteString m ()
    loop = do
      bs <-
        MIO.liftIO $
          FTP.recv h defaultChunkSize
            `M.catchIOError` const (return "")
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
