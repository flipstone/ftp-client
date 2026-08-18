module Main (main) where

import Network.FTP.Client
import qualified Network.FTP.Client.Conduit as FC
import Conduit
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = withFTPS "hostname.com" 21 $ \h _welcome -> do
    _ <- login h "username" "password"
    runConduitRes
        $ FC.mlsd h "."
        .| mapC (C.pack . (<> "\n") . show)
        .| stdoutC
