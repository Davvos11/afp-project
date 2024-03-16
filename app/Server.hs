{-# LANGUAGE OverloadedStrings #-}
module Server (runServer) where
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Middleware.Cors
import Data.Aeson

import ExampleData

runServer :: IO ()
runServer = do
        let port = 3000
        putStrLn $ "Listening on port " ++ show port
        run port $ simpleCors app


app :: Application
app req respond = do
  print req
  case pathInfo req of
    ["stops"]  -> respond $ responseLBS status200 [(hContentType, "application/json")] $ encode stopData
    ["buses"]  -> respond $ responseLBS status200 [(hContentType, "application/json")] $ encode busData
    ["delays"] -> {- case parseRequest (queryString req) of
                       Nothing      -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Couldn't parse request" --: Ideally you'd say here what didn't correctly get parsed/what was missing entirely
                       Just request -> getServerData >>= \serverData -> if not $ validateDelayRequest request serverData
                                       then respond $ responseLBS status404 [(hContentType, "text/plain")] "Data parsed but requests data not in the database"
                                       else calculationWithServerData >>= (\data -> respond $ responseLBS status200 [(hContentType, "application/json")] $ encode data)
                  -}respond $ responseLBS status200 [(hContentType, "application/json")] $ encode delayData
    _          -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Unsupported request"