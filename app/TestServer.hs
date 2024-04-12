{-# LANGUAGE OverloadedStrings #-}
module TestServer where

import Data.Aeson
import ExampleData
import MessageFormat
import Network.HTTP.Types
import Network.Wai

-- | Server that responds with example data
testApp :: Application
testApp req respond = do
  print req
  case pathInfo req of
    ["stops"] ->
        respond $ responseLBS status200 [(hContentType, "application/json")] $ encode stopData
    ["buses"] ->
        respond $ responseLBS status200 [(hContentType, "application/json")] $ encode busData
    ["delays"] -> respond $ responseLBS status200 [(hContentType, "application/json")] $ encode delayData