{-# LANGUAGE OverloadedStrings #-}

module Subscriber (run) where

-- Contains code from
-- https://github.com/jaspervdj/websockets/blob/master/example/client.hs

import           Control.Monad       (forever)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import           Data.ByteString.Lazy as BS
import           Data.Text.Lazy.Encoding as TLE
import           Data.Text.Lazy as TL
import qualified Codec.Compression.GZip as GZip

run :: IO ()
run = withSocketsDo $ WS.runClient "localhost" 9160 "/" app

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    
    forever $ do
        msg <- WS.receiveDataMessage conn

        -- For converting ByteString to Text or String, see
        -- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring

        case msg of
            WS.Binary a -> do
                case BS.head a of
                    0x1f -> do
                        let decompressed = GZip.decompress a
                        let decompressedText = (TL.toStrict $ TLE.decodeUtf8 decompressed)
                        liftIO $ T.putStrLn ("Received binary-encoded compressed text data: " <> T.take 30 decompressedText <> "...")

                    _ -> liftIO $ T.putStrLn ("Received binary-encoded text data: " <> (TL.toStrict $ TLE.decodeUtf8 a))
            WS.Text a _ -> do
                liftIO $ T.putStrLn $ ("Receive text data: " <> (TL.toStrict $ TLE.decodeUtf8 a))
            

        pure ()
    -- WS.sendClose conn ("Exit" :: Text)



