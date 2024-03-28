{-# LANGUAGE OverloadedStrings #-}

module Subscriber (run) where

-- Contains code from
-- https://github.com/jaspervdj/websockets/blob/master/example/client.hs

import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Control.Monad       (forever, filterM)
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
import           Text.XML.Light.Input (parseXMLDoc)
import qualified Text.XML.Light.Types as XML
import qualified Text.XML.Light.Proc as XMLProc
import Database.SQLite.Simple

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

posinfoElToMap :: XML.Element -> Map String String
posinfoElToMap el =
    let kv = ("type", (XML.qName . XML.elName) el):[
            ((XML.qName . XML.elName) b, XMLProc.strContent b) |
                a <- XML.elContent el,
                b <- case a of
                    XML.Elem c -> [c]
                    _ -> []
            ]
    in
        Map.fromList kv

kv6posinfoToUpdates :: BS.ByteString -> [Map String String]
kv6posinfoToUpdates rawS =
    let (Just content) = parseXMLDoc rawS
        posinfo = [
                d |
                    (XML.Elem a) <- XML.elContent content,
                    (XML.qName . XML.elName) a == "KV6posinfo",
                    b <- XML.elContent a,
                    d <- case b of
                        XML.Elem c -> [c]
                        _ -> []
            ]
    in
        Prelude.map posinfoElToMap posinfo

includeUpdate :: Map String String -> Bool
includeUpdate m =
    let t = m ! "type"
    in t == "DEPARTURE" || t == "ARRIVAL"

data StopInfo = StopInfo { stopCode :: String, name :: String,
gpsLat :: Float, gpsLon :: Float} deriving Show

instance FromRow StopInfo where
    fromRow = StopInfo <$> field <*> field <*> field <*> field

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    dbconn <- liftIO $ open "database2.db"
    
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

        let updates = (if (msgType == "compressed text data") then
                (kv6posinfoToUpdates msgData) else [])

        let updates' = Prelude.filter includeUpdate updates :: [Map String String]

        let isStopIncluded :: String -> IO Bool
            isStopIncluded stop = do
                result <- query dbconn "SELECT * FROM stops WHERE stop_code = ? LIMIT 1"
                            [stop] :: IO [StopInfo]
                -- let result = []
                return (Prelude.length result > 0)

        let includeUpdate' :: Map String String -> IO Bool
            includeUpdate' m = isStopIncluded (m ! "userstopcode")

        -- updates'' <- liftIO $ filterM (isStopIncluded . (Prelude.map (! "userstopcode"))) updates'
        updates'' <- liftIO $ filterM includeUpdate' updates'

        -- liftIO $ putStrLn $ show test

        -- liftIO $ T.putStrLn $ ("Received " <> msgType <> ": " <> limitText msgData)

        -- liftIO $ T.putStrLn $ show updates
        liftIO $ putStrLn $ show updates''

        pure ()

    close dbconn
    -- WS.sendClose conn ("Exit" :: Text)



