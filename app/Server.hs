{-# LANGUAGE OverloadedStrings #-}
module Server (runServer) where
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Middleware.Cors
import Data.Aeson

import ExampleData
import MessageFormat

-- | Runs a simple http wai server on port 3000. Messages get wrapped with CORS headers.
runServer :: IO ()
runServer = do
        let port = 3000
        putStrLn $ "Listening on port " ++ show port
        run port $ simpleCors app

-- | The actual http server
app :: Application
app req respond = do
  print req
  case pathInfo req of
    -- Frontend initialization, requests for the stops and buses as encoded in the database
    -- Example data for now
    ["stops"]  -> respond $ responseLBS status200 [(hContentType, "application/json")] $ encode stopData
    ["buses"]  -> respond $ responseLBS status200 [(hContentType, "application/json")] $ encode busData
    -- A delay calculation request
    ["delays"] -> case parseDelayRequest (queryString req) of
                       Nothing      -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Couldn't parse request" -- *Ideally* you'd say here what didn't correctly get parsed/what was missing from the request
                       Just request -> -- For now, just respond with example data. When the database impl. is good, use request data instead, and 404 if the data is nonsense
                                       {-getServerData >>= \serverData -> if not $ validateDelayRequest request serverData
                                       then respond $ responseLBS status404 [(hContentType, "text/plain")] "Data parsed but requests data not in the database"
                                       else calculationWithServerData >>= (\data -> respond $ responseLBS status200 [(hContentType, "application/json")] $ encode data) -}
                                       respond $ responseLBS status200 [(hContentType, "application/json")] $ encode delayData
    _          -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Unsupported request"