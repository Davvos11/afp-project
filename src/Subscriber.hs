{-# LANGUAGE OverloadedStrings #-}

module Subscriber (run) where

-- Contains code from
-- https://github.com/jaspervdj/websockets/blob/master/example/client.hs

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Text.Encoding as TSE
import Data.Text.Lazy.Encoding as TLE
import Data.ByteString.Lazy as BL
import Data.Text.Lazy as TL

run :: IO ()
run = withSocketsDo $ WS.runClient "localhost" 9160 "/" app

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveDataMessage conn

        -- let firstByte = BS.head msg
        -- case firstByte of
        --     0x1f -> liftIO $ putStrLn "Received binary data"
        --     _ -> liftIO $ T.putStrLn msg

        -- For converting ByteString to Text or String, see
        -- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring

        case msg of
            -- WS.Binary (0x1f `cons` 0x8b `cons` xs) -> do
            WS.Binary a -> do
                case BS.head a of
                    0x1f -> liftIO $ putStrLn "Received binary data"
                    -- b -> liftIO $ T.putStrLn ("Received binary-encoded text data: " ++ show b)
                    _ -> liftIO $ T.putStrLn ("Received binary-encoded text data: " <> (TL.toStrict $ TLE.decodeUtf8 a))
            WS.Text a _ -> do
                liftIO $ T.putStrLn $ ("Receive text data: " <> (TL.toStrict $ TLE.decodeUtf8 a))
            

        -- \x1f\x8b
        -- let a = WS.toLazyByteString msg
        pure ()

        -- liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop
    loop
    -- WS.sendClose conn ("Exit" :: Text)



