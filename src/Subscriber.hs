{-# LANGUAGE OverloadedStrings #-}

module Subscriber (run) where

-- Contains code from
-- https://github.com/jaspervdj/websockets/blob/master/example/client.hs

import           Control.Monad       (forever)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import           Data.ByteString.Lazy as BS
import           Data.Text.Lazy.Encoding as TLE
import           Data.Text.Lazy as TL
import qualified Codec.Compression.GZip as GZip

run :: IO ()
run = withSocketsDo $ WS.runClient "localhost" 9160 "/" app

limitText :: BS.ByteString -> Data.Text.Text
limitText s =
    let a = T.take 31 . TL.toStrict . TLE.decodeUtf8 $ s
    in (
        if (T.length a) > 30
        then a <> "..."
        else a
       ) 

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    
    forever $ do
        msg <- WS.receiveDataMessage conn

        -- For converting ByteString to Text or String, see
        -- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring

        let (msgType, msgData) = (
                case msg of
                WS.Binary a ->
                    case BS.head a of
                        0x1f ->
                            let decompressed = GZip.decompress a
                            in ("compressed text data", decompressed)

                        _ -> ("text data", a)

                WS.Text a _ -> ("text data (direct)", a)
                )
        liftIO $ T.putStrLn $ ("Received " <> msgType <> ": " <> limitText msgData)

        pure ()
    -- WS.sendClose conn ("Exit" :: Text)



