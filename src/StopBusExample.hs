{-# LANGUAGE DeriveGeneric #-}
module StopBusExample (stopData, busData) where

import Data.Aeson
import GHC.Generics

data StopPair = StopPair {stopName :: String, stopId :: Int} deriving Generic
data Stops = Stops {stops :: [StopPair]} deriving Generic
data BusPair = BusPair {busName :: String, busId :: Int} deriving Generic
data Buses = Buses {buses :: [BusPair]} deriving Generic

instance ToJSON StopPair where
instance ToJSON Stops where
instance ToJSON BusPair where
instance ToJSON Buses where

exampleStops :: [(String, Int)]
exampleStops = [("P+R Science Park", 0),
                ("WKZ / Máxima", 1),
                ("UMC Utrecht", 2),
                ("Heidelberglaan", 3),
                ("Botanische Tuinen", 4),
                ("Rijnsweerd-Zuid", 5),
                ("Rijnsweerd-Noord", 6)
                ]

toStopPair :: (String, Int) -> StopPair
toStopPair (s, i) = StopPair s i

stopData :: Stops
stopData = Stops $ map toStopPair exampleStops

exampleBuses :: [(String, Int)]
exampleBuses = [("28", 0),
                ("202", 1),
                ("200", 2),
                ("30", 3),
                ("34", 5)
               ]

toBusPair :: (String, Int) -> BusPair
toBusPair (s, i) = BusPair s i

busData :: Buses
busData = Buses $ map toBusPair exampleBuses