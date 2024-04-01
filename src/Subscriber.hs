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
import Database.SQLite.Simple

run :: IO ()
-- run = withSocketsDo $ WS.runClient "localhost" 9160 "/" app
-- run = withSocketsDo $ WS.runClient "localhost" 9160 "/" listen'
-- run = do
--     _ <- listen
--     return ()
--     -- listen >>= (\_ -> ())
run = do
    -- runConduit $ listen .| C.mapM_ (liftIO . print)
    -- runConduit $ listen .| printPosInfo
    listen printPosInfo
    -- return ()

printPosInfo :: ConduitT PosInfo Void IO ()
printPosInfo = do
    posinfo <- await
    case posinfo of
        Nothing -> return ()
        Just a -> do
            liftIO $ print a
            printPosInfo
        

type PosInfo = Map String String

-- data Stream a = Cons a (Stream a)

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

getStop :: Connection -> String -> IO (Maybe StopInfo)
getStop dbconn stopcode = do
    stops <- query dbconn "SELECT * FROM stops WHERE stop_code = ? LIMIT 1"
                            [stopcode] :: IO [StopInfo]
    pure $ case stops of
        [] -> Nothing
        x:_ -> Just x
    

includeUpdate :: Connection -> PosInfo -> IO Bool
includeUpdate dbconn m =
    -- let t = m ! "type"
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
        
    -- in (&&) <$> pure (t == "DEPARTURE" || t == "ARRIVAL")
    -- <*> (isJust <$> (getStop dbconn (
    --     case (Map.lookup "userstopcode" m) of
    --         Just a -> a
    --         Nothing -> error $ "Missing userstopcode in " ++ show m)))


data StopInfo = StopInfo {
        stopCode :: String, name :: String,
        gpsLat :: Float, gpsLon :: Float
    } deriving Show

instance FromRow StopInfo where
    fromRow = StopInfo <$> field <*> field <*> field <*> field

decompressMessage :: WS.DataMessage -> (String, BS.ByteString)
decompressMessage (WS.Text a _)
    = ("text data (direct)", a)
decompressMessage (WS.Binary a) | (BS.head a) == 0x1f
    = ("compressed data", GZip.decompress a)
decompressMessage (WS.Binary a)
    = ("data", a)

-- listen :: IO (Stream PosInfo)
-- listen = withSocketsDo $ WS.runClient "localhost" 9160 "/" listen'

-- listen :: ConduitT PosInfo Void IO r -> IO r
-- listen downstream =
--     WS.runClient "localhost" 9160 "/" (
--         \conn -> C.runConduit $ (listen' conn) .| downstream
--     )


--  -> ConduitT a Void IO ()
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
    dbconn <- liftIO $ open "database2.db"
    filterConduit (includeUpdate dbconn) .| dst
    -- close dbconn

-- filterPosInfo downstream = do
--     posinfo <- await
--     case posinfo of
--         Nothing -> return ()
--         Just a -> do
--             include <- liftIO $ includeUpdate dbconn a
--             if include then do
--                 yield a
--                 filterPosInfo downstream
--             else
--                 filterPosInfo downstream

listen :: ConduitT PosInfo Void IO () -> IO ()
listen downstream = listenKV6 (filterPosInfo downstream)

    -- listenKV6 (do
    --     dbconn <- liftIO $ open "database2.db"
    --     _ <- forever $ do
    --         posinfo <- await
    --         case posinfo of
    --             Nothing -> return ()
    --             Just a -> do
    --                 include <- liftIO $ includeUpdate dbconn a
    --                 if include then yield a else return ()
    --     pure()
    -- )

-- listen'' :: (PosInfo -> IO ()) -> WS.ClientApp ()
-- listen'' _ conn = do
--     liftIO $ listen' conn

-- data PosInfoStream = 

-- listenPosInfo :: PosInfoStream a -> IO a

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
                -- updates' <- liftIO $ filterM (includeUpdate dbconn) updates
                mapM_ yield updates
            _ -> return ()
    pure ()


-- listen' :: WS.Connection -> ConduitT () PosInfo IO ()
-- listen' conn = do
--     liftIO $ putStrLn "Connected!"

--     dbconn <- liftIO $ open "database2.db"

--     _ <- forever $ do
--         message <- liftIO $ WS.receiveDataMessage conn

--         case decompressMessage message of
--             ("compressed data", msgData) -> do
--                 let updates = extractPosInfo msgData
--                 updates' <- liftIO $ filterM (includeUpdate dbconn) updates
--                 mapM_ yield updates'
--             _ -> return ()

--         -- let decompressed :: BS.ByteString
--         --     decompressed = snd $ decompressMessage message

--         -- error "test"
--     pure ()

    -- let messages' :: IO (Stream WS.DataMessage)
    --     messages' = (Cons <$> WS.receiveDataMessage conn <*> messages')

    -- liftIO $ putStrLn "aa"
    -- messages <- messages'
    -- liftIO $ putStrLn "bb"

    -- let decompressed :: Stream (BS.ByteString)
    --     decompressed = 
    --         Stream.map snd $
    --         Stream.filter (\(t, _) -> t == "compressed data") $
    --         Stream.map decompressMessage messages
    
    -- let updates :: Stream PosInfo
    --     updates = 

    -- error $ show decompressed


app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    dbconn <- liftIO $ open "database2.db"
    
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

    close dbconn
    -- WS.sendClose conn ("Exit" :: Text)



