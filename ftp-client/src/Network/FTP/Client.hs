{- |
Module      : Network.FTP.Client
Description : Transfer files over FTP and FTPS
Copyright   : Megan Robinson 2018-2019, Flipstone Technology Partners 2024-2026
License     : Public Domain
Stability   : experimental
Portability : POSIX
-}
module Network.FTP.Client
  ( -- * Main Entrypoints
    withFTP
  , withFTPS
  , withFTPSSettings

    -- * Control Commands
  , login
  , pasv
  , rename
  , dele
  , cwd
  , size
  , acct
  , mkd
  , rmd
  , pwd
  , quit

    -- * Data Commands
  , nlst
  , retr
  , list
  , stor
  , mlsd
  , mlst

    -- * Types
  , FTPCommand (..)
  , FTPResponse (..)
  , FTPMessage (..)
  , ResponseStatus (..)
  , MlsxResponse (..)
  , RTypeCode (..)
  , PortActivity (..)
  , ProtType (..)
  , Security (..)
  , TLSContext (..)
  , Handle (..)

    -- * TLS Commands
  , pbsz
  , prot

    -- * Exceptions
  , FTPException (..)

    -- * System Handle Creation
  , createSIOHandle
  , createTLSConnection
  , connectTLS

    -- * Handle Implementations
  , sIOHandleImpl
  , tlsHandleImpl

    -- * Lower Level Functions
  , sendCommand
  , sendCommandS
  , recvAll
  , sendAll
  , sendAllS
  , getLineResp
  , getAllLineResp
  , getLineRespMaybe
  , maxReplyLineLength
  , toNetworkAscii
  , requireTLSContext
  , getResponse
  , getResponseS
  , sendCommandLine
  , createSendDataCommand
  , createTLSSendDataCommand
  , PendingCompletion
  , newPendingCompletion
  , drainPendingCompletion
  , parseMlsxLine
  ) where

import Control.Arrow ((***))
import qualified Control.Exception as Exception
import Control.Monad ((<=<))
import qualified Control.Monad as Monad
import Control.Monad.Catch (MonadCatch, MonadMask)
import qualified Control.Monad.Catch as M
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Default.Class (def)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Network.Connection as Connection
import qualified Network.Socket as S
import qualified System.IO as SIO
import System.IO.Error (eofErrorType, isEOFError, mkIOError)

debugging :: Bool
debugging = False

debugPrint :: (Show a, MIO.MonadIO m) => a -> m ()
debugPrint s = Monad.when debugging (MIO.liftIO $ print s)

debugResponse :: (Show a, MIO.MonadIO m) => a -> m ()
debugResponse s = debugPrint $ "Recieved: " <> show s

{- | What a data connection needs in order to be protected the same way the
control connection is. The host is the one the control connection was opened
to, which is what the server's certificate is issued for -- deriving it from
the data socket instead names the local end and can never validate.
-}
data TLSContext = TLSContext
  { tlsContextSettings :: Connection.TLSSettings
  , tlsContextHost :: String
  , tlsContextPort :: Int
  }

-- Only TLS settings are threaded through the connection helpers today. If the
-- rest of the hardening lands -- ignoring the address in a PASV reply, a
-- timeout on the active-mode accept, a bound on data-channel line length --
-- those belong together with these as fields of one options record passed to
-- the with* functions, rather than as further positional parameters. That was
-- @qxjit's suggestion on https://github.com/flipstone/ftp-client/pull/1.

data Security = Clear | TLS TLSContext

-- | Can send and recieve a 'Data.ByteString.ByteString'.
data Handle = Handle
  { send :: ByteString -> IO ()
  , sendLine :: ByteString -> IO ()
  , recv :: Int -> IO ByteString
  , recvLine :: IO ByteString
  , security :: Security
  }

data FTPMessage = SingleLine ByteString | MultiLine [ByteString]
  deriving (Eq)

instance Show FTPMessage where
  show (SingleLine message) = C.unpack message
  show (MultiLine messages) = intercalate "\n" $ C.unpack <$> messages

-- | Response from an FTP command. ex "200 Welcome!"
data FTPResponse = FTPResponse
  { frStatus :: ResponseStatus
  -- ^ Interpretation of the first digit of an FTP response code
  , frCode :: Int
  -- ^ The three digit response code
  , frMessage :: FTPMessage
  -- ^ Text of the response
  }
  deriving (Eq)

instance Show FTPResponse where
  show fr = show (frCode fr) <> " " <> show (frMessage fr)

-- | First digit of an FTP response
data ResponseStatus
  = -- | 1
    Wait
  | -- | 2
    Success
  | -- | 3
    Continue
  | -- | 4
    FailureRetry
  | -- | 5
    Failure
  deriving (Show, Eq)

data FTPException
  = FailureRetryException FTPResponse
  | FailureException FTPResponse
  | UnsuccessfulException FTPResponse
  | BogusResponseFormatException FTPResponse
  | BadProtocolResponseException ByteString
  deriving (Show)

instance Exception.Exception FTPException

responseStatus :: ByteString -> ResponseStatus
responseStatus cbs =
  case C.uncons cbs of
    Just ('1', _) -> Wait
    Just ('2', _) -> Success
    Just ('3', _) -> Continue
    Just ('4', _) -> FailureRetry
    Just ('5', _) -> Failure
    _ -> Exception.throw $ BadProtocolResponseException cbs

data RTypeCode = TA | TI

serialzeRTypeCode :: RTypeCode -> String
serialzeRTypeCode TA = "A"
serialzeRTypeCode TI = "I"

data PortActivity = Active | Passive

data ProtType = P | C

-- | Commands according to the FTP specification
data FTPCommand
  = User String
  | Pass String
  | Acct String
  | RType RTypeCode
  | Retr String
  | Nlst [String]
  | Port S.HostAddress S.PortNumber
  | Stor String
  | List [String]
  | Rnfr String
  | Rnto String
  | Dele String
  | Size String
  | Mkd String
  | Rmd String
  | Pbsz Int
  | Prot ProtType
  | Mlsd String
  | Mlst String
  | Cwd String
  | Cdup
  | Ccc
  | Auth
  | Pwd
  | Abor
  | Pasv
  | Quit

instance Show FTPCommand where
  show = serializeCommand

formatPort :: S.HostAddress -> S.PortNumber -> String
formatPort ha pn =
  let
    (w1, w2, w3, w4) = S.hostAddressToTuple ha
    hn = show <$> [w1, w2, w3, w4]
    portParts = show <$> [pn `quot` 256, pn `mod` 256]
  in
    intercalate "," (hn <> portParts)

serializeCommand :: FTPCommand -> String
serializeCommand (User user) = "USER " <> user
serializeCommand (Pass pass) = "PASS " <> pass
serializeCommand (Acct account) = "ACCT " <> account
serializeCommand (RType rt) = "TYPE " <> serialzeRTypeCode rt
serializeCommand (Retr file) = "RETR " <> file
serializeCommand (Nlst []) = "NLST"
serializeCommand (Nlst args) = "NLST " <> unwords args
serializeCommand (Port ha pn) = "PORT " <> formatPort ha pn
serializeCommand (Stor loc) = "STOR " <> loc
serializeCommand (List []) = "LIST"
serializeCommand (List args) = "LIST " <> unwords args
serializeCommand (Rnfr from) = "RNFR " <> from
serializeCommand (Rnto to) = "RNTO " <> to
serializeCommand (Dele file) = "DELE " <> file
serializeCommand (Size file) = "SIZE " <> file
serializeCommand (Mkd dir) = "MKD " <> dir
serializeCommand (Rmd dir) = "RMD " <> dir
serializeCommand (Pbsz buf) = "PBSZ " <> show buf
serializeCommand (Prot P) = "PROT P"
serializeCommand (Prot C) = "PROT C"
serializeCommand (Mlsd path) = "MLSD " <> path
serializeCommand (Mlst path) = "MLST " <> path
serializeCommand (Cwd dir) = "CWD " <> dir
serializeCommand Cdup = "CDUP"
serializeCommand Ccc = "CCC"
serializeCommand Auth = "AUTH TLS"
serializeCommand Pwd = "PWD"
serializeCommand Abor = "ABOR"
serializeCommand Pasv = "PASV"
serializeCommand Quit = "QUIT"

{- | Cap on one reply line. RFC 959 replies are short; this exists so a server
that never sends a newline cannot make us buffer without limit. It applies to
both control connections: 'Connection.connectionGetLine' takes it directly, and
'hGetLineBounded' applies it to a clear handle. Exceeding it raises
'Connection.LineTooLong', which is not an 'IOError' and so is not swallowed by
the end-of-input handling elsewhere in this module.
-}
maxReplyLineLength :: Int
maxReplyLineLength = 65536

{- | 'C.hGetLine' with a bound, for handles that 'Network.Connection' is not
managing. Reads a byte at a time rather than in chunks because 'recv' shares
the handle: anything taken past the newline would be stolen from the data the
caller asks for next. The handle is buffered, so this is not a syscall per
byte, and reply lines are short.

End of input is reported the way 'C.hGetLine' reports it, because
'getLineRespMaybe' relies on the distinction: throw when nothing has been read
yet, hand back the partial line when something has.
-}
hGetLineBounded :: Int -> SIO.Handle -> IO ByteString
hGetLineBounded limit h =
  let
    collect :: Int -> [ByteString] -> IO ByteString
    collect remaining acc
      | remaining <= 0 = Exception.throwIO Connection.LineTooLong
      | otherwise = do
          byte <- B.hGet h 1
          if B.null byte
            then
              if null acc
                then
                  Exception.throwIO $
                    mkIOError eofErrorType "hGetLineBounded" (Just h) Nothing
                else return $ B.concat (reverse acc)
            else
              if byte == C.singleton '\n'
                then return $ B.concat (reverse acc)
                else collect (remaining - 1) (byte : acc)
  in
    collect limit []

stripCLRF :: ByteString -> ByteString
stripCLRF = C.takeWhile $ (&&) <$> (/= '\r') <*> (/= '\n')

-- | Get a line from the server
getLineResp :: Handle -> IO ByteString
getLineResp h = stripCLRF <$> recvLine h

{- | Get a line from the server, returning 'Nothing' once the stream is
exhausted. A blank line and end of input are different things: 'recvLine'
signals end of input by throwing, and an empty 'ByteString' is a legitimate
line of reply text.
-}
getLineRespMaybe :: Handle -> IO (Maybe ByteString)
getLineRespMaybe h =
  (Just <$> getLineResp h) `M.catchIOError` \e ->
    if isEOFError e
      then return Nothing
      else ioError e

{- | Get a full response from the server
Used in 'sendCommand'
-}
getResponse :: MIO.MonadIO m => Handle -> m FTPResponse
getResponse h = do
  line <- MIO.liftIO $ getLineResp h
  let
    (code, rest) = C.splitAt 3 line
  -- A response must open with a three digit code. Checking that up front keeps
  -- the 'C.uncons' below and the 'read' further down from being partial.
  Monad.when (C.length code < 3 || not (C.all AC.isDigit code)) $
    MIO.liftIO $
      Exception.throwIO $
        BadProtocolResponseException line
  message <- case C.uncons rest of
    Just ('-', _) -> MultiLine <$> loopMultiLine h code [line]
    _ -> return $ SingleLine line
  let
    codeDroppedMessage = case message of
      SingleLine singleMessage -> SingleLine $ C.drop 4 singleMessage
      MultiLine [] -> MultiLine []
      MultiLine (firstMessage : messages) ->
        MultiLine $ C.drop 4 firstMessage : messages
  let
    response =
      FTPResponse
        (responseStatus code)
        (read $ C.unpack code)
        codeDroppedMessage
  case frStatus response of
    FailureRetry -> MIO.liftIO $ Exception.throwIO $ FailureRetryException response
    Failure -> MIO.liftIO $ Exception.throwIO $ FailureException response
    _ -> return response

loopMultiLine ::
  MIO.MonadIO m =>
  Handle ->
  ByteString ->
  [ByteString] ->
  m [ByteString]
loopMultiLine h code priorLines = do
  mNextLine <- MIO.liftIO $ getLineRespMaybe h
  case mNextLine of
    -- The server hung up before sending the terminating line. Stop rather
    -- than looping forever, but treat the reply as bad rather than
    -- returning it: what was collected is a fragment, and handing it back
    -- would turn a truncated reply into a well formed one. A cut off "220-"
    -- greeting would read as a successful 220 and let 'withFTP' carry on
    -- against a control connection that is already gone.
    --
    -- This is end of input, not a blank line. RFC 959 lets the intermediate
    -- lines of a multiline reply hold arbitrary text, blank lines included,
    -- so a blank line has to be kept and the loop has to continue past it.
    Nothing ->
      MIO.liftIO $
        Exception.throwIO $
          BadProtocolResponseException $
            C.intercalate "\n" priorLines
    Just nextLine -> do
      -- RFC 959 (https://datatracker.ietf.org/doc/html/rfc959#page-36) ends a
      -- multiline reply with the code followed by a space, and continues it
      -- with the code followed by a hyphen. The bare code is accepted too,
      -- for servers that omit the trailing space on an empty final line.
      let
        newLines = priorLines <> [C.dropWhile (== ' ') nextLine]
        isLastLine =
          nextLine == code
            || C.isPrefixOf (code <> " ") nextLine
      if isLastLine
        then return newLines
        else loopMultiLine h code newLines

ensureSuccess :: MIO.MonadIO m => FTPResponse -> m FTPResponse
ensureSuccess resp =
  case frStatus resp of
    Success -> return resp
    _ -> MIO.liftIO $ Exception.throwIO $ UnsuccessfulException resp

getResponseS :: MIO.MonadIO m => Handle -> m FTPResponse
getResponseS = ensureSuccess <=< getResponse

sendCommandLine :: MIO.MonadIO m => Handle -> ByteString -> m ()
sendCommandLine h = MIO.liftIO . send h . (<> "\r\n")

{- | Send a command to the server and get a response back.
Some commands use a data 'Handle', and their data is not returned here.
-}
sendCommand :: MIO.MonadIO m => Handle -> FTPCommand -> m FTPResponse
sendCommand h fc = do
  let
    command = serializeCommand fc
  debugPrint $ "Sending: " <> command
  sendCommandLine h $ C.pack command
  resp <- getResponse h
  debugResponse resp
  return resp

sendCommandS :: MIO.MonadIO m => Handle -> FTPCommand -> m FTPResponse
sendCommandS h fc = sendCommand h fc >>= ensureSuccess

{- | Equvalent to

> mapM . sendCommand
-}
sendAll :: MIO.MonadIO m => Handle -> [FTPCommand] -> m [FTPResponse]
sendAll = mapM . sendCommand

{- | Equvalent to

> mapM . sendCommandS
-}
sendAllS :: MIO.MonadIO m => Handle -> [FTPCommand] -> m [FTPResponse]
sendAllS = mapM . sendCommandS

-- Control connection

createSocket ::
  MIO.MonadIO m =>
  Maybe String ->
  Int ->
  S.AddrInfo ->
  m (S.Socket, S.AddrInfo)
createSocket host portNum hints = do
  addr <- MIO.liftIO $ do
    a : _ <- S.getAddrInfo (Just hints) host (Just $ show portNum)
    return a
  debugPrint $ "Addr: " <> show addr
  sock <-
    MIO.liftIO $
      S.socket
        (S.addrFamily addr)
        (S.addrSocketType addr)
        (S.addrProtocol addr)
  return (sock, addr)

withSocketPassive ::
  (MIO.MonadIO m, MonadMask m) =>
  String ->
  Int ->
  (S.Socket -> m a) ->
  m a
withSocketPassive host portNum f = do
  let
    hints =
      S.defaultHints
        { S.addrSocketType = S.Stream
        }
  -- bracketOnError, not bracket: on success this socket is handed to
  -- socketToHandle, which takes ownership of the descriptor, so closing it here
  -- as well would be wrong. Contrast withSocketActive above.
  M.bracketOnError
    (createSocket (Just host) portNum hints)
    (MIO.liftIO . S.close . fst)
    ( \(sock, addr) -> do
        debugPrint ("Connecting" :: String)
        MIO.liftIO $ S.connect sock (S.addrAddress addr)
        debugPrint ("Connected" :: String)
        f sock
    )

withSocketActive :: (MIO.MonadIO m, MonadMask m) => (S.Socket -> m a) -> m a
withSocketActive f = do
  let
    hints =
      S.defaultHints
        { S.addrSocketType = S.Stream
        , S.addrFlags = [S.AI_PASSIVE]
        }
  -- bracket, not bracketOnError: in active mode acceptData returns a *new*
  -- socket and only that one is converted to a Handle, so ownership of this
  -- listening socket is never transferred and it must be closed on the success
  -- path too. The passive helper below is the opposite case and deliberately
  -- differs.
  M.bracket
    (createSocket Nothing 0 hints)
    (MIO.liftIO . S.close . fst)
    ( \(sock, addr) -> do
        debugPrint ("Binding" :: String)
        MIO.liftIO $ S.bind sock (S.addrAddress addr)
        MIO.liftIO $ S.listen sock 1
        debugPrint ("Listening" :: String)
        f sock
    )

createSIOHandle :: (MIO.MonadIO m, MonadMask m) => String -> Int -> m SIO.Handle
createSIOHandle host portNum =
  withSocketPassive host portNum $
    MIO.liftIO . flip S.socketToHandle SIO.ReadWriteMode

sIOHandleImpl :: SIO.Handle -> Handle
sIOHandleImpl h =
  Handle
    { send = C.hPut h
    , -- RFC 959 TYPE A data uses CRLF, not a bare LF
      sendLine = \s -> C.hPut h (s <> "\r\n")
    , recv = C.hGetSome h
    , recvLine = hGetLineBounded maxReplyLineLength h
    , security = Clear
    }

withSIOHandle ::
  (MIO.MonadIO m, MonadMask m) =>
  String ->
  Int ->
  (Handle -> m a) ->
  m a
withSIOHandle host portNum f =
  M.bracket
    (MIO.liftIO $ createSIOHandle host portNum)
    (MIO.liftIO . SIO.hClose)
    (f . sIOHandleImpl)

{- | Takes a host name and port. A handle for interacting with the server
will be returned in a callback.

@
withFTP "ftp.server.com" 21 $ \h welcome -> do
    print welcome
    login h "username" "password"
    print =<< nlst h []
@
-}
withFTP ::
  (MIO.MonadIO m, MonadMask m) =>
  String ->
  Int ->
  (Handle -> FTPResponse -> m a) ->
  m a
withFTP host portNum f = withSIOHandle host portNum $ \h -> do
  resp <- getResponse h
  f h resp

-- Data connection

withDataSocketPasv ::
  (MIO.MonadIO m, MonadMask m) =>
  Handle ->
  (S.Socket -> m a) ->
  m a
withDataSocketPasv h f = do
  (host, portNum) <- pasv h
  debugPrint $ "Host: " <> host
  debugPrint $ "Port: " <> show portNum
  withSocketPassive host portNum f

withDataSocketActive ::
  (MIO.MonadIO m, MonadMask m) =>
  Handle ->
  (S.Socket -> m a) ->
  m a
withDataSocketActive h f = withSocketActive $ \socket -> do
  (sPort, sHost) <- MIO.liftIO $ do
    (S.SockAddrInet p hostAddr) <- S.getSocketName socket
    return (p, hostAddr)
  _ <- port h sHost sPort
  f socket

-- | Open a socket that can be used for data transfers
withDataSocket ::
  (MIO.MonadIO m, MonadMask m) =>
  PortActivity ->
  Handle ->
  (S.Socket -> m a) ->
  m a
withDataSocket Active = withDataSocketActive
withDataSocket Passive = withDataSocketPasv

acceptData :: MIO.MonadIO m => PortActivity -> S.Socket -> m S.Socket
acceptData Passive = return
acceptData Active = return . fst <=< MIO.liftIO . S.accept

-- Response to data commands should be 150 but apparently
-- some servers will respond with 200 before 150 so just ignore it
ensureSucessfulData :: MIO.MonadIO m => Handle -> FTPResponse -> m ()
ensureSucessfulData h resp = do
  resp' <- case frStatus resp of
    Success -> do
      newResp <- getResponse h
      debugResponse newResp
      return newResp
    _ -> return resp
  MIO.liftIO $
    Monad.when (frStatus resp' /= Wait) $
      Exception.throwIO $
        UnsuccessfulException resp

{- | Send setup commands to the server and
create a data 'System.IO.Handle'
-}
createSendDataCommand ::
  (MIO.MonadIO m, MonadMask m) =>
  Handle ->
  PortActivity ->
  PendingCompletion ->
  FTPCommand ->
  m SIO.Handle
createSendDataCommand h pa pending cmd = withDataSocket pa h $ \socket -> do
  resp <- sendCommand h cmd
  ensureSucessfulData h resp
  -- Past this point the server has accepted the transfer, so a completion
  -- reply is owed to us however the rest of it goes.
  markCompletionPending pending
  acceptedSock <- acceptData pa socket
  MIO.liftIO $ S.socketToHandle acceptedSock SIO.ReadWriteMode

-- | Provides a data 'Handle' in a callback for a command
withDataCommand ::
  (MIO.MonadIO m, MonadMask m) =>
  Handle ->
  PortActivity ->
  RTypeCode ->
  FTPCommand ->
  (Handle -> m a) ->
  m a
withDataCommand ch pa code cmd f = do
  _ <- sendCommandS ch $ RType code
  pending <- newPendingCompletion
  x <-
    M.bracket
      (createSendDataCommand ch pa pending cmd)
      (MIO.liftIO . SIO.hClose)
      (f . sIOHandleImpl)
      `M.onException` drainPendingCompletion ch pending
  resp <- getResponse ch
  debugResponse resp
  return x

-- | Recieve data and interpret it linewise
getAllLineResp :: (MIO.MonadIO m, MonadCatch m) => Handle -> m ByteString
getAllLineResp h =
  let
    collect :: MIO.MonadIO n => [ByteString] -> n ByteString
    collect ret = do
      mLine <- MIO.liftIO $ getLineRespMaybe h
      case mLine of
        Nothing -> return $ C.intercalate (C.pack "\n") ret
        Just line -> collect (ret <> [line])
  in
    collect []

-- | Recieve all data and return it as a 'Data.ByteString.ByteString'
recvAll :: (MIO.MonadIO m, MonadCatch m) => Handle -> m ByteString
recvAll h =
  let
    collect :: (MIO.MonadIO n, MonadCatch n) => ByteString -> n ByteString
    collect bs =
      ( do
          -- No handler here on purpose. recv returns "" at end of data, so
          -- catching IOErrors would only turn a reset or timed-out connection
          -- into a short read that looks like a complete one.
          chunk <- MIO.liftIO $ recv h defaultChunkSize
          if C.null chunk
            then return bs
            else collect $ bs <> chunk
      )
  in
    collect ""

-- TLS connection

{- | Wrap an existing handle in TLS. The settings are supplied by the caller;
'def' validates the server's certificate chain and host name. Passing settings
with 'Connection.settingDisableCertificateValidation' set gives an encrypted
but unauthenticated connection, which any on-path attacker can read and rewrite.
-}
connectTLS ::
  MIO.MonadIO m =>
  Connection.TLSSettings ->
  SIO.Handle ->
  String ->
  Int ->
  m Connection.Connection
connectTLS settings h host portNum = do
  context <- MIO.liftIO Connection.initConnectionContext
  let
    connectionParams =
      Connection.ConnectionParams
        { Connection.connectionHostname = host
        , Connection.connectionPort = toEnum . fromEnum $ portNum
        , Connection.connectionUseSecure = Just settings
        , Connection.connectionUseSocks = Nothing
        }
  MIO.liftIO $ Connection.connectFromHandle context h connectionParams

createTLSConnection ::
  (MIO.MonadIO m, MonadMask m) =>
  Connection.TLSSettings ->
  String ->
  Int ->
  m (FTPResponse, Connection.Connection)
createTLSConnection settings host portNum =
  -- Without this the socket leaks whenever the greeting is a refusal, AUTH TLS
  -- is rejected, or the handshake fails. This is the acquire action of
  -- withTLSHandle's bracket, so its release would never run.
  M.bracketOnError
    (createSIOHandle host portNum)
    (MIO.liftIO . SIO.hClose)
    ( \h -> do
        let
          insecureH = sIOHandleImpl h
        resp <- getResponse insecureH
        _ <- sendCommandS insecureH Auth
        conn <- connectTLS settings h host portNum
        return (resp, conn)
    )

tlsHandleImpl :: TLSContext -> Connection.Connection -> Handle
tlsHandleImpl tlsContext c =
  Handle
    { send = Connection.connectionPut c
    , -- RFC 959 TYPE A data uses CRLF, not a bare LF
      sendLine = Connection.connectionPut c . (<> "\r\n")
    , recv = Connection.connectionGet c
    , recvLine = Connection.connectionGetLine maxReplyLineLength c
    , security = TLS tlsContext
    }

withTLSHandle ::
  (MonadMask m, MIO.MonadIO m) =>
  Connection.TLSSettings ->
  String ->
  Int ->
  (Handle -> FTPResponse -> m a) ->
  m a
withTLSHandle settings host portNum f =
  let
    tlsContext =
      TLSContext
        { tlsContextSettings = settings
        , tlsContextHost = host
        , tlsContextPort = portNum
        }
  in
    M.bracket
      (createTLSConnection settings host portNum)
      (MIO.liftIO . Connection.connectionClose . snd)
      (\(resp, conn) -> f (tlsHandleImpl tlsContext conn) resp)

{- | Takes a host name and port. A handle for interacting with the server
will be returned in a callback. The connection is protected with TLS and the
server's certificate chain and host name are verified, so a failure to validate
aborts the connection.

@
withFTPS "ftps.server.com" 21 $ \h welcome -> do
    print welcome
    login h "username" "password"
    print =<< nlst h []
@

Use 'withFTPSSettings' if you need to talk to a server whose certificate cannot
be validated.
-}
withFTPS ::
  (MonadMask m, MIO.MonadIO m) =>
  String ->
  Int ->
  (Handle -> FTPResponse -> m a) ->
  m a
withFTPS = withTLSHandle def

{- | As 'withFTPS', but with caller supplied TLS settings.

Setting 'Connection.settingDisableCertificateValidation' accepts any
certificate, including one an attacker generated. The connection is then
encrypted but not authenticated: anyone on the network path can read the
credentials sent by 'login' and alter transferred data. Only do this when you
have another way to establish that the peer is who it claims to be.

@
let insecure = 'def' { 'Connection.settingDisableCertificateValidation' = True }
withFTPSSettings insecure "ftps.server.com" 21 $ \h welcome -> do
    print welcome
@
-}
withFTPSSettings ::
  (MonadMask m, MIO.MonadIO m) =>
  Connection.TLSSettings ->
  String ->
  Int ->
  (Handle -> FTPResponse -> m a) ->
  m a
withFTPSSettings = withTLSHandle

-- TLS data connection

{- | The TLS details of a control connection, for reproducing them on a data
connection.
-}
requireTLSContext :: MIO.MonadIO m => Handle -> m TLSContext
requireTLSContext h =
  case security h of
    TLS ctx -> return ctx
    Clear ->
      MIO.liftIO . Exception.throwIO . BadProtocolResponseException $
        C.pack "cannot open a TLS data connection over a clear control connection"

{- | Read the reply that terminates a data transfer, discarding any failure.

Used on the paths where the transfer did not complete normally. The reply still
has to be taken off the control connection: left there it becomes the answer to
whichever command is sent next, and every reply after that belongs to the
previous command for the rest of the session. Failures are swallowed so this
cannot mask the exception that brought us here.
-}
drainDataResponse :: (MIO.MonadIO m, MonadCatch m) => Handle -> m ()
drainDataResponse ch =
  Monad.void (getResponse ch) `M.catchAll` (\_ -> return ())

{- | Whether the server has accepted the preliminary reply to a transfer
command, and so whether a completion reply is on its way.

A transfer that is rejected outright -- @PBSZ@, @PROT@, @PASV@ or the transfer
command itself -- fails with its error reply already consumed by
'ensureSucessfulData', and nothing further is coming. Draining then waits for a
reply the server will never send, which blocks until it gives up on the
connection. Draining is only correct once this says a reply is pending.
-}
newtype PendingCompletion = PendingCompletion (IORef Bool)

newPendingCompletion :: MIO.MonadIO m => m PendingCompletion
newPendingCompletion =
  MIO.liftIO $ PendingCompletion <$> newIORef False

markCompletionPending :: MIO.MonadIO m => PendingCompletion -> m ()
markCompletionPending (PendingCompletion ref) =
  MIO.liftIO $ writeIORef ref True

{- | 'drainDataResponse', but only when a completion reply is actually pending.

This deliberately stays outside the bracket that owns the data connection. The
server sends the completion reply once that connection has closed, so draining
before the release action has run would block on a reply that is waiting on us.
-}
drainPendingCompletion ::
  (MIO.MonadIO m, MonadCatch m) =>
  Handle ->
  PendingCompletion ->
  m ()
drainPendingCompletion ch (PendingCompletion ref) = do
  pending <- MIO.liftIO $ readIORef ref
  Monad.when pending $ drainDataResponse ch

{- | Send setup commands to the server and
create a data TLS connection
-}
createTLSSendDataCommand ::
  (MIO.MonadIO m, MonadMask m) =>
  Handle ->
  PortActivity ->
  PendingCompletion ->
  FTPCommand ->
  m Connection.Connection
createTLSSendDataCommand ch pa pending cmd = do
  tlsContext <- requireTLSContext ch
  _ <- sendAllS ch [Pbsz 0, Prot P]
  withDataSocket pa ch $ \socket -> do
    resp <- sendCommand ch cmd
    ensureSucessfulData ch resp
    -- Past this point the server has accepted the transfer, so a completion
    -- reply is owed to us however the rest of it goes -- a rejected
    -- certificate in the handshake below included.
    markCompletionPending pending
    acceptedSock <- acceptData pa socket
    -- socketToHandle invalidates the socket, so the enclosing bracketOnError's
    -- close becomes a no-op from here on; protect the handle separately or a
    -- failed handshake leaks the descriptor.
    M.bracketOnError
      (MIO.liftIO $ S.socketToHandle acceptedSock SIO.ReadWriteMode)
      (MIO.liftIO . SIO.hClose)
      ( \h ->
          -- Authenticate against the host the control connection was opened to.
          -- getSocketName here would name the local end of the data socket,
          -- which no server certificate can ever match.
          MIO.liftIO $
            connectTLS
              (tlsContextSettings tlsContext)
              h
              (tlsContextHost tlsContext)
              (tlsContextPort tlsContext)
      )

withTLSDataCommand ::
  (MIO.MonadIO m, MonadMask m) =>
  Handle ->
  PortActivity ->
  RTypeCode ->
  FTPCommand ->
  (Handle -> m a) ->
  m a
withTLSDataCommand ch pa code cmd f = do
  tlsContext <- requireTLSContext ch
  _ <- sendCommandS ch $ RType code
  pending <- newPendingCompletion
  x <-
    M.bracket
      (createTLSSendDataCommand ch pa pending cmd)
      (MIO.liftIO . Connection.connectionClose)
      (f . tlsHandleImpl tlsContext)
      `M.onException` drainPendingCompletion ch pending
  resp <- getResponse ch
  debugPrint $ "Recieved: " <> show resp
  return x

parseResponse :: MIO.MonadIO m => FTPResponse -> AC.Parser a -> m a
parseResponse resp p =
  let
    parsableMessage = case frMessage resp of
      SingleLine message -> message
      MultiLine messages -> C.intercalate "\n" messages
  in
    case AC.parseOnly p parsableMessage of
      Right x -> return x
      Left _ ->
        MIO.liftIO $
          Exception.throwIO $
            BadProtocolResponseException parsableMessage

ensureCode :: MIO.MonadIO m => FTPResponse -> Int -> m ()
ensureCode resp code =
  MIO.liftIO $
    Monad.when (frCode resp /= code) $
      MIO.liftIO $
        Exception.throwIO $
          UnsuccessfulException resp

parse227 :: AC.Parser (String, Int)
parse227 = do
  _ <- AC.skipWhile (/= '(') *> AC.char '('
  [h1, h2, h3, h4, p1, p2] <- AC.many1 AC.digit `AC.sepBy` AC.char ','
  let
    host = intercalate "." [h1, h2, h3, h4]
    highBits = read p1
    lowBits = read p2
    portNum = (highBits `Bits.shift` 8) + lowBits
  return (host, portNum)

parse257 :: AC.Parser String
parse257 = do
  _ <- AC.char '"'
  C.unpack <$> AC.takeTill (== '"')

-- Control commands

login :: MIO.MonadIO m => Handle -> String -> String -> m FTPResponse
login h user pass = do
  resp <- last <$> sendAll h [User user, Pass pass]
  ensureSuccess resp

pasv :: MIO.MonadIO m => Handle -> m (String, Int)
pasv h = do
  resp <- sendCommandS h Pasv
  ensureCode resp 227
  parseResponse resp parse227

port :: MIO.MonadIO m => Handle -> S.HostAddress -> S.PortNumber -> m FTPResponse
port h ha pn = sendCommandS h (Port ha pn)

acct :: MIO.MonadIO m => Handle -> String -> m FTPResponse
acct h pass = sendCommandS h (Acct pass)

rename :: MIO.MonadIO m => Handle -> String -> String -> m FTPResponse
rename h from to = do
  res <- sendCommand h (Rnfr from)
  case frStatus res of
    Continue -> sendCommandS h (Rnto to)
    _ -> return res

dele :: MIO.MonadIO m => Handle -> String -> m FTPResponse
dele h file = sendCommandS h (Dele file)

cwd :: MIO.MonadIO m => Handle -> String -> m FTPResponse
cwd h dir =
  sendCommandS h $
    if dir == ".."
      then Cdup
      else Cwd dir

size :: MIO.MonadIO m => Handle -> String -> m Int
size h file = do
  resp <- sendCommandS h (Size file)
  ensureCode resp 213
  return $ case frMessage resp of
    SingleLine message -> read . C.unpack $ message
    MultiLine _ -> 0

mkd :: MIO.MonadIO m => Handle -> String -> m String
mkd h dir = do
  resp <- sendCommandS h (Mkd dir)
  ensureCode resp 257
  parseResponse resp parse257

rmd :: MIO.MonadIO m => Handle -> String -> m FTPResponse
rmd h dir = sendCommandS h (Rmd dir)

pwd :: MIO.MonadIO m => Handle -> m String
pwd h = do
  resp <- sendCommandS h Pwd
  ensureCode resp 257
  parseResponse resp parse257

quit :: MIO.MonadIO m => Handle -> m FTPResponse
quit h = sendCommandS h Quit

mlst :: (MIO.MonadIO m, MonadMask m) => Handle -> String -> m MlsxResponse
mlst h path = do
  resp <- sendCommandS h (Mlst path)
  case frMessage resp of
    SingleLine message -> return $ parseMlsxLine message
    MultiLine messages ->
      if length messages >= 2
        then return $ parseMlsxLine $ messages !! 1
        else MIO.liftIO $ Exception.throwIO $ BogusResponseFormatException resp

-- TLS commands

pbsz :: MIO.MonadIO m => Handle -> Int -> m FTPResponse
pbsz h = sendCommandS h . Pbsz

prot :: MIO.MonadIO m => Handle -> ProtType -> m FTPResponse
prot h = sendCommandS h . Prot

-- CCC and AUTH deliberately have no wrappers. CCC tells the server to drop to
-- cleartext, but there is no way to downgrade our side of a
-- crypton-connection, so the control connection would desynchronise and
-- 'security' could not be corrected to match. AUTH on its own tells the server
-- to expect a handshake that never happens; 'createTLSConnection' is the only
-- sequence that issues it correctly. Both remain reachable as 'FTPCommand'
-- constructors for anyone who needs to drive them by hand.

-- Data commands

{- | Rewrite line endings for a TYPE A transfer. RFC 959 specifies NVT-ASCII,
whose terminator is CRLF. Input already using CRLF is left alone rather than
having its CR doubled, and input with no final terminator does not gain one.
-}
toNetworkAscii :: ByteString -> ByteString
toNetworkAscii =
  let
    dropTrailingCR piece =
      if not (C.null piece) && C.last piece == '\r'
        then C.init piece
        else piece
  in
    C.intercalate (C.pack "\r\n") . fmap dropTrailingCR . C.split '\n'

sendType :: MIO.MonadIO m => RTypeCode -> ByteString -> Handle -> m ()
sendType TA dat h = MIO.liftIO . send h $ toNetworkAscii dat
sendType TI dat h = MIO.liftIO $ send h dat

withDataCommandSecurity ::
  (MIO.MonadIO m, MonadMask m) =>
  Handle ->
  PortActivity ->
  RTypeCode ->
  FTPCommand ->
  (Handle -> m a) ->
  m a
withDataCommandSecurity h =
  case security h of
    Clear -> withDataCommand h
    TLS _ -> withTLSDataCommand h

nlst :: (MIO.MonadIO m, MonadMask m) => Handle -> [String] -> m ByteString
nlst h args = withDataCommandSecurity h Passive TA (Nlst args) getAllLineResp

retr :: (MIO.MonadIO m, MonadMask m) => Handle -> String -> m ByteString
retr h path = withDataCommandSecurity h Passive TI (Retr path) recvAll

list :: (MIO.MonadIO m, MonadMask m) => Handle -> [String] -> m ByteString
list h args = withDataCommandSecurity h Passive TA (List args) getAllLineResp

stor ::
  (MIO.MonadIO m, MonadMask m) =>
  Handle ->
  String ->
  B.ByteString ->
  RTypeCode ->
  m ()
stor h loc dat rtype =
  withDataCommandSecurity h Passive rtype (Stor loc) $ sendType rtype dat

data MlsxResponse = MlsxResponse
  { mrFilename :: String
  , mrFacts :: Map String String
  }
  deriving (Show)

splitApart :: Char -> ByteString -> (ByteString, ByteString)
splitApart on s =
  let
    (x0, x1) = C.break (== on) s
  in
    (x0, C.drop 1 x1)

parseMlsxLine :: ByteString -> MlsxResponse
parseMlsxLine line =
  let
    (factLine, filename) = splitApart ' ' line
    bFacts = splitApart '=' <$> C.split ';' factLine
    facts =
      Map.fromList $
        filter (not . null . fst) $
          Monad.join (***) C.unpack <$> bFacts
  in
    MlsxResponse (C.unpack filename) facts

getMlsxResponse :: (MIO.MonadIO m, MonadCatch m) => Handle -> m [MlsxResponse]
getMlsxResponse h =
  let
    collect :: MIO.MonadIO n => [MlsxResponse] -> n [MlsxResponse]
    collect ret = do
      mLine <- MIO.liftIO $ getLineRespMaybe h
      case mLine of
        Nothing -> return ret
        Just line ->
          collect $
            if C.null line
              then ret
              else parseMlsxLine line : ret
  in
    collect []

mlsd :: (MIO.MonadIO m, MonadMask m) => Handle -> String -> m [MlsxResponse]
mlsd h path = withDataCommandSecurity h Passive TA (Mlsd path) getMlsxResponse
