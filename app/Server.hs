{-# LANGUAGE OverloadedStrings #-}
module Server (runServer) where
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import Data.Aeson

import StopBusExample

runServer :: IO ()
runServer = do
        let port = 3000
        putStrLn $ "Listening on port " ++ show port
        run port app


app :: Application
app req respond = respond $ case pathInfo req of
    ["stops"] -> responseLBS status200 [(hContentType, "application/json")] $ encode stopData
    ["buses"] -> responseLBS status200 [(hContentType, "application/json")] $ encode busData
    _         -> responseLBS status200 [(hContentType, "text/plain")] "Hello World!"