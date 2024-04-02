{-# LANGUAGE DuplicateRecordFields #-}

module Database.Models (ActualArrival(..), Frequency(..)) where

import Database.SQLite.Simple

data ActualArrival = ActualArrival { timestamp :: Int, stopId :: Int, punctuality :: Int, journeyId :: Int} deriving Show

instance FromRow ActualArrival where
    fromRow = ActualArrival <$> field <*> field <*> field <*> field

data Frequency = Frequency { lineNumber :: String, punctuality :: Int, frequency :: Int} deriving Show

instance FromRow Frequency where
    fromRow = Frequency <$> field <*> field <*> field
