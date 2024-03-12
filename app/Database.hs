{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.SQLite.Simple

data ActualArrival = ActualArrival { timestamp :: Int, stopId :: Int, punctuality :: Int, journeyId :: Int} deriving Show

instance FromRow ActualArrival where
    fromRow = ActualArrival <$> field <*> field <*> field <*> field

data Frequency = Frequency { value :: Int, frequency :: Int} deriving Show

instance FromRow Frequency where
    fromRow = Frequency <$> field <*> field

-- | Example function to select all data
getData :: IO [ActualArrival]
getData = do
    conn <- open "database.db"
    result <- query_ conn "SELECT * FROM actual_arrivals" :: IO [ActualArrival]
    close conn
    return result

-- | Get the frequency distribution of the punctualities for a given bus stop
getFrequenciesForBusStop :: Int -> IO [Frequency]
getFrequenciesForBusStop stop = do
    conn <- open "database.db"
    result <- query conn "SELECT punctuality, COUNT(*) AS frequency \
                            \FROM actual_arrivals \
                            \WHERE journey_id = ? \
                            \GROUP BY punctuality"
                            (Only stop) :: IO [Frequency]
    close conn
    return result
