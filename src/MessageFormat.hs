{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module MessageFormat where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Types
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.List
import Text.Read

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

validWeekday :: String -> Maybe Weekday
validWeekday "0" = Just Mon
validWeekday "1" = Just Tue
validWeekday "2" = Just Wed
validWeekday "3" = Just Thu
validWeekday "4" = Just Fri
validWeekday "5" = Just Sat
validWeekday "6" = Just Sun
validWeekday  _  = Nothing

validHour :: Int -> Maybe Int
validHour h | h >= 0 && h < 24 = Just h
            | otherwise        = Nothing

validMinute :: Int -> Maybe Int
validMinute h | h >= 0 && h < 60 = Just h
            | otherwise        = Nothing

-- Todo, also temp location
type ServerData = ()

parseDelayRequest :: Query -> Maybe DelayRequest
parseDelayRequest q = do
  qDeparture   <- find ((== "departure")   . fst) q >>= snd >>= Just . unpack . decodeUtf8 >>= readMaybe @Int
  qDestination <- find ((== "destination") . fst) q >>= snd >>= Just . unpack . decodeUtf8 >>= readMaybe @Int
  qBus         <- find ((== "bus")         . fst) q >>= snd >>= Just . unpack . decodeUtf8 >>= readMaybe @Int
  qWeekday     <- find ((== "weekday")     . fst) q >>= snd >>= validWeekday . unpack . decodeUtf8
  qHour        <- find ((== "hour")        . fst) q >>= snd >>= Just . unpack . decodeUtf8 >>= readMaybe @Int >>= validHour
  qMinute      <- find ((== "minute")      . fst) q >>= snd >>= Just . unpack . decodeUtf8 >>= readMaybe @Int >>= validMinute
  return $ DelayRequest qDeparture qDestination qBus qWeekday qHour qMinute

validateDelayRequest :: DelayRequest -> ServerData -> Bool
validateDelayRequest r sd = undefined