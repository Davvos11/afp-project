{-# LANGUAGE OverloadedStrings #-}
module Server (runServer) where
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Middleware.Cors
import Data.Aeson

import StopBusExample

runServer :: IO ()
runServer = do
        let port = 3000
        putStrLn $ "Listening on port " ++ show port
        run port $ simpleCors app


app :: Application
app req respond = do
  print req
  respond $ case pathInfo req of
    ["stops"] -> responseLBS status200 [(hContentType, "application/json")] $ encode stopData
    ["buses"] -> responseLBS status200 [(hContentType, "application/json")] $ encode busData
    -- Todo add a case here for delay calculation requests
    _         -> responseLBS status200 [(hContentType, "text/plain")] "Hello World!"