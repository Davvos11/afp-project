{-# LANGUAGE DeriveGeneric #-}
module MessageFormat where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Types

data StopPair = StopPair {stopName :: String, stopId :: Int} deriving Generic
data Stops = Stops {stops :: [StopPair]} deriving Generic
instance ToJSON StopPair where
instance ToJSON Stops where

toStopPair :: (String, Int) -> StopPair
toStopPair (s, i) = StopPair s i

data BusPair = BusPair {busName :: String, busId :: Int} deriving Generic
data Buses = Buses {buses :: [BusPair]} deriving Generic
instance ToJSON BusPair where
instance ToJSON Buses where

toBusPair :: (String, Int) -> BusPair
toBusPair (s, i) = BusPair s i

data Delays = Delays {departureDelay :: Int, destinationDelay :: Int} deriving Generic
instance ToJSON Delays where

data DelayRequest = DelayRequest {departure :: Int, destination :: Int, bus :: Int, weekday :: Weekday, hour :: Int, minute :: Int}

-- Temp location for this type
data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
-- Todo, also temp location
type ServerData = ()

parseDelayRequest :: Query -> Maybe DelayRequest
parseDelayRequest q = undefined

validateDelayRequest :: DelayRequest -> ServerData -> Bool
validateDelayRequest r sd = undefined