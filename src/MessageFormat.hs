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
import Data.Time.Calendar

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

data DelayRequest = DelayRequest {departure :: Int, destination :: Int, bus :: Int, weekday :: DayOfWeek, hour :: Int, minute :: Int}

-- | Tries to parse a string to a weekday according to the encoded representation.
-- Week starts at Monday, index 0, and so on.
validWeekday :: String -> Maybe DayOfWeek
validWeekday "0" = Just Monday
validWeekday "1" = Just Tuesday
validWeekday "2" = Just Wednesday
validWeekday "3" = Just Thursday
validWeekday "4" = Just Friday
validWeekday "5" = Just Saturday
validWeekday "6" = Just Sunday
validWeekday  _  = Nothing

-- | Turns numbers outside the valid hour range (0-23) into Nothing.
validHour :: Int -> Maybe Int
validHour h | h >= 0 && h < 24 = Just h
            | otherwise        = Nothing

-- | Turns 'Int's outside the valid minute range (0-59) into Nothing.
validMinute :: Int -> Maybe Int
validMinute h | h >= 0 && h < 60 = Just h
            | otherwise        = Nothing

-- Todo, also temp location
type ServerData = ()

parseDelayRequest :: Query -> Maybe DelayRequest
parseDelayRequest q = do
  qDeparture   <- find ((== "departure")   . fst) q >>= snd >>= readMaybe @Int . unpack . decodeUtf8
  qDestination <- find ((== "destination") . fst) q >>= snd >>= readMaybe @Int . unpack . decodeUtf8
  qBus         <- find ((== "bus")         . fst) q >>= snd >>= readMaybe @Int . unpack . decodeUtf8
  qWeekday     <- find ((== "weekday")     . fst) q >>= snd >>= validWeekday   . unpack . decodeUtf8
  qHour        <- find ((== "hour")        . fst) q >>= snd >>= readMaybe @Int . unpack . decodeUtf8 >>= validHour
  qMinute      <- find ((== "minute")      . fst) q >>= snd >>= readMaybe @Int . unpack . decodeUtf8 >>= validMinute
  return $ DelayRequest qDeparture qDestination qBus qWeekday qHour qMinute

validateDelayRequest :: DelayRequest -> ServerData -> Bool
validateDelayRequest r sd = undefined