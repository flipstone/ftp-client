module Main (main) where

import Control.Concurrent.MVar
import qualified Control.Exception as Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Network.Connection as Connection
import Network.FTP.Client hiding (Success)
import qualified Network.FTP.Client as F
import qualified System.Directory as Directory
import qualified System.IO as SIO
import System.IO.Error (eofErrorType, fullErrorType, isFullError, mkIOError)
import Test.Hspec

data TestHandleMVars = TestHandleMVars
  { thmSend :: MVar [ByteString]
  , _thmSendLine :: MVar [ByteString]
  , _thmRecv :: MVar [Int]
  }

data TestHandle = TestHandle TestHandleMVars Handle

testHandle ::
  [ByteString] ->
  [ByteString] ->
  Security ->
  IO TestHandle
testHandle recvResps recvLineResps sec = do
  sendMVar <- newMVar []
  sendLineMVar <- newMVar []
  recvMVar <- newMVar []
  recvCount <- newMVar 0
  recvLineCount <- newMVar 0
  let
    testHandleMVars =
      TestHandleMVars
        sendMVar
        sendLineMVar
        recvMVar
    handle =
      Handle
        { send = \s ->
            modifyMVar_
              sendMVar
              (\ss -> return $ ss <> [s])
        , sendLine = \s ->
            modifyMVar_
              sendLineMVar
              (\ss -> return $ ss <> [s])
        , recv = \i -> do
            modifyMVar_
              recvMVar
              (\is -> return $ is <> [i])
            nextScripted "recv" recvResps recvCount
        , recvLine = nextScripted "recvLine" recvLineResps recvLineCount
        , security = sec
        }
  return $ TestHandle testHandleMVars handle

{- | Hand back the next scripted response, or signal end of input the way a
real handle does once the peer has hung up.
-}
nextScripted :: String -> [ByteString] -> MVar Int -> IO ByteString
nextScripted what scripted countMVar = do
  i <- modifyMVar countMVar (\count -> return (count + 1, count))
  case drop i scripted of
    (x : _) -> return x
    [] -> ioError $ mkIOError eofErrorType what Nothing Nothing

{- | A handle whose reads fail the way a reset connection does, rather than the
way end of input does. The two must not be conflated: end of input is a
complete transfer, a reset is a truncated one.
-}
failingHandle :: Security -> IO Handle
failingHandle sec = do
  (TestHandle _ h) <- testHandle [] [] sec
  return
    h
      { recv = \_ -> ioError brokenConnection
      , recvLine = ioError brokenConnection
      }

brokenConnection :: IOError
brokenConnection = mkIOError fullErrorType "connection reset" Nothing Nothing

main :: IO ()
main = hspec $ do
  describe "Network.FTP.Client.sendCommand" $ do
    it "sends USER for User" $ do
      let
        expected =
          FTPResponse
            F.Success
            200
            (SingleLine $ C.pack "Ok")
      (TestHandle mvars h) <- testHandle [] [C.pack "200 Ok"] Clear
      sendCommand h (User "megan") `shouldReturn` expected
      takeMVar (thmSend mvars) `shouldReturn` [C.pack "USER megan\r\n"]
    it "sends USER for User and receives a multiline response" $ do
      let
        expected =
          FTPResponse
            F.Success
            200
            (MultiLine [C.pack "line1", C.pack "line2", C.pack "200 line3"])
      (TestHandle mvars h) <-
        testHandle
          []
          [ C.pack "200-line1\r\n"
          , C.pack "line2\r\n"
          , C.pack "200 line3\r\n"
          ]
          Clear
      sendCommand h (User "megan") `shouldReturn` expected
      takeMVar (thmSend mvars) `shouldReturn` [C.pack "USER megan\r\n"]
  describe "Network.FTP.Client.getResponse" $ do
    it "rejects an empty response line" $ do
      (TestHandle _ h) <- testHandle [] [C.pack ""] Clear
      getResponse h `shouldThrow` isBadProtocolResponse
    it "rejects a response line with a non numeric code" $ do
      (TestHandle _ h) <- testHandle [] [C.pack "abc def"] Clear
      getResponse h `shouldThrow` isBadProtocolResponse
    it "rejects a response line with a truncated code" $ do
      (TestHandle _ h) <- testHandle [] [C.pack "20 Ok"] Clear
      getResponse h `shouldThrow` isBadProtocolResponse
    it "accepts a bare code with no message" $ do
      let
        expected =
          FTPResponse
            F.Success
            200
            (SingleLine $ C.pack "")
      (TestHandle _ h) <- testHandle [] [C.pack "200"] Clear
      getResponse h `shouldReturn` expected
    it "keeps a blank line inside a multiline response" $ do
      -- RFC 959 lets the intermediate lines carry arbitrary text, so a
      -- blank line is reply content and must not end the response. Ending
      -- early would leave the real terminator unread and every later
      -- command would pick up the wrong reply.
      let
        expected =
          FTPResponse
            F.Success
            220
            ( MultiLine
                [ C.pack "First Line"
                , C.pack ""
                , C.pack "220 Third Line"
                ]
            )
      (TestHandle _ h) <-
        testHandle
          []
          [ C.pack "220-First Line\r\n"
          , C.pack "\r\n"
          , C.pack "220 Third Line\r\n"
          ]
          Clear
      getResponse h `shouldReturn` expected
    it "keeps reading continuation lines that repeat the code" $ do
      let
        expected =
          FTPResponse
            F.Success
            220
            ( MultiLine
                [ C.pack "First Line"
                , C.pack "220-Second Line"
                , C.pack "220 Third Line"
                ]
            )
      (TestHandle _ h) <-
        testHandle
          []
          [ C.pack "220-First Line\r\n"
          , C.pack "220-Second Line\r\n"
          , C.pack "220 Third Line\r\n"
          ]
          Clear
      getResponse h `shouldReturn` expected
    it "ends a multiline response on a bare code" $ do
      let
        expected =
          FTPResponse
            F.Success
            220
            (MultiLine [C.pack "First Line", C.pack "220"])
      (TestHandle _ h) <-
        testHandle
          []
          [ C.pack "220-First Line\r\n"
          , C.pack "220\r\n"
          ]
          Clear
      getResponse h `shouldReturn` expected
    it "does not end a multiline response on a different code" $ do
      let
        expected =
          FTPResponse
            F.Success
            220
            ( MultiLine
                [ C.pack "First Line"
                , C.pack "331 Not the terminator"
                , C.pack "220 Done"
                ]
            )
      (TestHandle _ h) <-
        testHandle
          []
          [ C.pack "220-First Line\r\n"
          , C.pack "331 Not the terminator\r\n"
          , C.pack "220 Done\r\n"
          ]
          Clear
      getResponse h `shouldReturn` expected
    it "rejects a multiline response the server never finished" $ do
      -- Terminating rather than hanging is only half of it. Handing back
      -- the fragment would report a successful 220 for a greeting that
      -- was cut off, against a control connection that is already gone.
      (TestHandle _ h) <-
        testHandle
          []
          [ C.pack "220-First Line\r\n"
          ]
          Clear
      getResponse h `shouldThrow` isBadProtocolResponse
  describe "Network.FTP.Client.recvAll" $ do
    it "doesn't hang on empty response" $ do
      let
        expected = C.pack ""
      (TestHandle _ h) <- testHandle [C.pack ""] [] Clear
      recvAll h `shouldReturn` expected
    it "reports a broken connection instead of a short read" $ do
      -- A failure part way through a transfer used to be turned into a clean
      -- end of data, so a truncated download could not be told apart from a
      -- complete one.
      h <- failingHandle Clear
      recvAll h `shouldThrow` isFullError
  describe "Network.FTP.Client.getAllLineResp" $
    it "reports a broken connection instead of a truncated listing" $ do
      h <- failingHandle Clear
      getAllLineResp h `shouldThrow` isFullError
  describe "Network.FTP.Client.sIOHandleImpl" $ do
    it "reads reply lines from a clear handle" $
      withBytesHandle (C.pack "220 Welcome\r\n331 Password\r\n") $ \h -> do
        getLineResp h `shouldReturn` C.pack "220 Welcome"
        getLineResp h `shouldReturn` C.pack "331 Password"
    it "hands back a final line the server never terminated" $
      -- Matches hGetLine, which this replaces: bytes did arrive, so they are
      -- the line, and end of input is reported on the read after it.
      withBytesHandle (C.pack "220 Welcome") $ \h -> do
        getLineResp h `shouldReturn` C.pack "220 Welcome"
        getLineRespMaybe h `shouldReturn` Nothing
    it "signals end of input once the stream is exhausted" $
      withBytesHandle (C.pack "220 Welcome\r\n") $ \h -> do
        _ <- getLineResp h
        getLineRespMaybe h `shouldReturn` Nothing
    it "refuses a reply line that exceeds the cap" $
      -- A server that never sends a newline must not be able to make us
      -- buffer without limit before we have even authenticated.
      withBytesHandle (C.replicate (maxReplyLineLength + 1) 'x') $ \h ->
        getLineResp h `shouldThrow` isLineTooLong
  describe "Network.FTP.Client.toNetworkAscii" $ do
    it "terminates LF input with CRLF" $
      toNetworkAscii (C.pack "a\nb\n") `shouldBe` C.pack "a\r\nb\r\n"
    it "leaves CRLF input unchanged rather than doubling the CR" $
      toNetworkAscii (C.pack "a\r\nb\r\n") `shouldBe` C.pack "a\r\nb\r\n"
    it "does not append a terminator the input did not have" $
      toNetworkAscii (C.pack "a\nb") `shouldBe` C.pack "a\r\nb"

{- | A real 'SIO.Handle' over fixed bytes. 'sIOHandleImpl' needs one, so the
scripted 'Handle' above cannot reach it.
-}
withBytesHandle :: ByteString -> (Handle -> IO a) -> IO a
withBytesHandle bytes use = do
  tmp <- Directory.getTemporaryDirectory
  Exception.bracket
    (SIO.openBinaryTempFile tmp "ftp-client-test")
    (\(path, h) -> SIO.hClose h >> Directory.removeFile path)
    ( \(path, h) -> do
        C.hPut h bytes
        SIO.hClose h
        SIO.withBinaryFile path SIO.ReadMode (use . sIOHandleImpl)
    )

isLineTooLong :: Connection.LineTooLong -> Bool
isLineTooLong _ = True

isBadProtocolResponse :: FTPException -> Bool
isBadProtocolResponse e =
  case e of
    BadProtocolResponseException _ -> True
    _ -> False
