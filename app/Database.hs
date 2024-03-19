{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.Models
import Database.SQLite.Simple
import qualified Data.Text as Text
import Text.Printf

data TimeOfDay = TimeOfDay { hours :: Int, minutes :: Int }
    deriving (Show)

timeToString :: TimeOfDay -> String
timeToString (TimeOfDay h m) = printf "%02d" h ++ ":" ++ printf "%02d" m ++ ":00"

data Filter = Filter {
    stopId :: Maybe Int,
    timeOfDay :: Maybe TimeOfDay
} deriving Show

-- | Get the frequency distribution of the punctualities for a given bus stop
getFrequencies :: Filter -> IO [Frequency]
getFrequencies (Filter mStopId mTimeOfDay) = do
    conn <- open "database.db"
    print queryString
    result <- queryNamed conn (Query queryString) parameters :: IO [Frequency]
    close conn
    return result
    where 
        baseQuery = "SELECT punctuality, COUNT(*) AS frequency \
                            \FROM actual_arrivals "
        
        (conditions, parameters) = foldr addFilter ([], []) [
                fmap (\s -> ("stop_id = :stop_id", Text.pack ":stop_id" := s)) mStopId,
                fmap (\t -> ("time(timestamp) >= time(:time_of_day_0, '-30 minutes')", ":time_of_day_0" := timeToString t)) mTimeOfDay,
                fmap (\t -> ("time(timestamp) <= time(:time_of_day_1, '+30 minutes')", ":time_of_day_1" := timeToString t)) mTimeOfDay
            ]

        addFilter Nothing acc = acc
        addFilter (Just (condition, parameter)) (condition_list, parameter_list) = (condition : condition_list, parameter: parameter_list)

        intermediateQuery = if null conditions
            then baseQuery
            else Text.concat [baseQuery, " WHERE ", Text.intercalate " AND " conditions]

        queryString = intermediateQuery <> " GROUP BY punctuality"
