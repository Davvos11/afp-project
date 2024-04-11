{-# LANGUAGE OverloadedStrings #-}

module Server (runServer) where

import Data.List (sortOn)
import Data.Aeson ( encode )
import Database (Filter (..), TimeOfDay (TimeOfDay), getFrequencies, getLines, getStops)
import Database.Models (Frequency (..))
import MessageFormat
    ( DelayRequest(DelayRequest),
      Delays(..),
      DelayFrequency(..),
      Buses(Buses),
      Stops(Stops),
      toStopPair,
      toBusPair,
      parseDelayRequest )
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors

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
    ["stops"] ->
      getStops >>= \ds ->
        respond $ responseLBS status200 [(hContentType, "application/json")] $ encode $ Stops $ map toStopPair ds
    ["buses"] ->
      getLines >>= \ls ->
        respond $ responseLBS status200 [(hContentType, "application/json")] $ encode $ Buses $ map toBusPair ls
    -- A delay calculation request
    ["delays"] -> case parseDelayRequest (queryString req) of
      Nothing -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Couldn't parse request"
      -- \*Ideally* you'd say here what didn't correctly get parsed/what was missing from the request
      Just request ->
        -- For now, just respond with example data. When the database impl. is good, use request data instead, and 404 if the data is nonsense
        getDelays request >>= \serverData ->
          respond $ responseLBS status200 [(hContentType, "application/json")] $ encode serverData
    {- if not $ validateDelayRequest request serverData
    then respond $ responseLBS status404 [(hContentType, "text/plain")] "Data parsed but requests data not in the database"
    else calculationWithServerData >>= (\d -> respond $ responseLBS status200 [(hContentType, "application/json")] $ encode d) -}
    --  respond $ responseLBS status200 [(hContentType, "application/json")] $ encode delayData
    _ -> respond $ responseLBS status400 [(hContentType, "text/plain")] "Unsupported request"

getDelays :: DelayRequest -> IO Delays
getDelays (DelayRequest departure destination bus weekday hour minute) = do
  start_delay <- get True
  end_delay <- get False
  return Delays {departureDelays = toDelayFrequencies start_delay, destinationDelays = toDelayFrequencies end_delay}
  where
    -- Would have added these two to MessageFormat.hs, but Frequency is the App source, not the Lib source
    toDelayFrequencies fs = sortOn punct $ map toDelayFrequency fs
    toDelayFrequency (Frequency _ p f) = DelayFrequency p f
    get get_start =
      getFrequencies get_start $
        Filter
          { startStop = Just departure,
            endStop = Just destination,
            linePlanningNumber = Just bus,
            timeOfDay = Just (TimeOfDay hour minute),
            dayOfWeek = Just weekday
          }

encodeDelayFrequency :: [Frequency] -> Int
encodeDelayFrequency [] = -1000
encodeDelayFrequency ((Frequency _ p _) : _) = p
