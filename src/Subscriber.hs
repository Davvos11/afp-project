{-# LANGUAGE OverloadedStrings #-}

module Subscriber (run) where

-- Contains code from
-- https://github.com/jaspervdj/websockets/blob/master/example/client.hs

import           Data.Map            (Map, (!))
import           Data.Maybe          (isJust)
import qualified Data.Map as Map
import qualified Data.Stream         as Stream
import           Data.Stream         (Stream (..))
import           Control.Monad       (forever, filterM, liftM)
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
import qualified Data.Conduit             as C
import           Data.Conduit
import qualified Database.SQLite.Simple   as SQL
import Database.SQLite.Simple.ToField (toField)

run :: IO ()
run = do
    listen writeToDatabase
    
printPosInfo :: ConduitT PosInfo Void IO ()
printPosInfo = do
    posinfo <- await
    case posinfo of
        Nothing -> return ()
        Just a -> do
            liftIO $ print a
            printPosInfo

writeToDatabase :: ConduitT PosInfo Void IO ()
writeToDatabase = do
    dbconn <- liftIO $ SQL.open "database2.db"
    liftIO $ SQL.execute_ dbconn
        "CREATE TABLE IF NOT EXISTS actual_arrivals ( \
        \    timestamp TEXT,\
        \    stop_code TEXT,\
        \    punctuality INTEGER,\
        \    journey_id INTEGER,\
        \    lineplanningnumber TEXT,\
        \    type TEXT\
        \)"

    posinfo <- await
    case posinfo of
        Nothing -> do
            liftIO $ SQL.close dbconn
            return ()

        Just a -> do
            liftIO $ SQL.execute dbconn
                "INSERT INTO actual_arrivals (timestamp, stop_code, \
                \punctuality, journey_id, lineplanningnumber, type) \
                \VALUES (?, ?, ?, ?, ?, ?)"
                [
                    toField (a ! "timestamp"),
                    toField (a ! "userstopcode"),
                    toField ((read (a ! "punctuality")) :: Int),
                    toField ((read (a ! "journeynumber")) :: Int),
                    toField (a ! "lineplanningnumber"),
                    toField (a ! "type")
                ]

            writeToDatabase


type PosInfo = Map String String

limitText :: BS.ByteString -> Data.Text.Text
limitText s =
    let a = T.take 31 . TL.toStrict . TLE.decodeUtf8 $ s
    in (
        if (T.length a) > 30
        then a <> "..."
        else a
       )

parseXMLposinfo :: XML.Element -> PosInfo
parseXMLposinfo el =
    let kv = ("type", (XML.qName . XML.elName) el):[
            ((XML.qName . XML.elName) b, XMLProc.strContent b) |
                a <- XML.elContent el,
                b <- case a of
                    XML.Elem c -> [c]
                    _ -> []
            ]
    in
        Map.fromList kv

extractPosInfo :: BS.ByteString -> [PosInfo]
extractPosInfo rawS =
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
        Prelude.map parseXMLposinfo posinfo

getStop :: SQL.Connection -> String -> IO (Maybe StopInfo)
getStop dbconn stopcode = do
    stops <- SQL.query dbconn "SELECT * FROM stops WHERE stop_code = ? LIMIT 1"
                            [stopcode] :: IO [StopInfo]
    pure $ case stops of
        [] -> Nothing
        x:_ -> Just x
    

includeUpdate :: SQL.Connection -> PosInfo -> IO Bool
includeUpdate dbconn m =
    let t = case (Map.lookup "type" m) of
            Just a -> a
            Nothing -> error $ "Missing type in " ++ show m
        isGoodType = t == "DEPARTURE" || t == "ARRIVAL"
    in
        case isGoodType of
            False -> pure False
            True -> do
                stop <- getStop dbconn (
                    case (Map.lookup "userstopcode" m) of
                        Just a -> a
                        Nothing -> error $ "Missing userstopcode in " ++ show m)
                pure $ isJust stop

data StopInfo = StopInfo {
        stopCode :: String, name :: String,
        gpsLat :: Float, gpsLon :: Float
    } deriving Show

instance SQL.FromRow StopInfo where
    fromRow = StopInfo <$> SQL.field <*> SQL.field <*> SQL.field <*> SQL.field

decompressMessage :: WS.DataMessage -> (String, BS.ByteString)
decompressMessage (WS.Text a _)
    = ("text data (direct)", a)
decompressMessage (WS.Binary a) | (BS.head a) == 0x1f
    = ("compressed data", GZip.decompress a)
decompressMessage (WS.Binary a)
    = ("data", a)

filterConduit :: (a -> IO Bool) -> ConduitT a a IO ()
filterConduit f = do
    a <- C.await
    case a of
        Nothing -> return ()
        Just a' -> do
            include <- liftIO $ f a'
            if include then do
                C.yield a'
                filterConduit f
            else
                filterConduit f

filterPosInfo :: (ConduitT PosInfo Void IO ()) -> ConduitT PosInfo Void IO ()
filterPosInfo dst = do
    dbconn <- liftIO $ SQL.open "database2.db"
    filterConduit (includeUpdate dbconn) .| dst

    -- Won't be reached if the conduit runs forever
    liftIO $ SQL.close dbconn


listen :: ConduitT PosInfo Void IO () -> IO ()
listen downstream = listenKV6 (filterPosInfo downstream)

listenKV6 :: ConduitT PosInfo Void IO r -> IO r
listenKV6 downstream =
    WS.runClient "localhost" 9160 "/" (
        \conn -> C.runConduit $ (listenKV6' conn) .| downstream
    )

listenKV6' :: WS.Connection -> ConduitT () PosInfo IO ()
listenKV6' conn = do
    _ <- forever $ do
        message <- liftIO $ WS.receiveDataMessage conn

        case decompressMessage message of
            ("compressed data", msgData) -> do
                let updates = extractPosInfo msgData
                mapM_ yield updates
            _ -> return ()
    pure ()


testing :: WS.ClientApp ()
testing conn = do
    putStrLn "Connected!"

    dbconn <- liftIO $ SQL.open "database2.db"
    
    forever $ do
        msg <- WS.receiveDataMessage conn

        -- For converting ByteString to Text or String, see
        -- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring


        let (msgType, msgData) = decompressMessage msg

        let updates = (if (msgType == "compressed data") then
                (extractPosInfo msgData) else [])

        updates' <- liftIO $ filterM (includeUpdate dbconn) updates

        liftIO $ putStrLn $ show updates'

        pure ()

    SQL.close dbconn
    -- WS.sendClose conn ("Exit" :: Text)



