{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Database where

import Database.Models
import Database.SQLite.Simple
import qualified Data.Text as Text
import Text.Printf

data TimeOfDay = TimeOfDay { hours :: Int, minutes :: Int }

instance Show TimeOfDay where
    show :: TimeOfDay -> String
    show (TimeOfDay h m) = printf "%02d" h ++ ":" ++ printf "%02d" m ++ ":00"

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Show DayOfWeek where
  show :: DayOfWeek -> String
  show Mon = "Monday"
  show Tue = "Tuesday"
  show Wed = "Wednesday"
  show Thu = "Thursday"
  show Fri = "Friday"
  show Sat = "Saturday"
  show Sun = "Sunday"

weekToNumber :: DayOfWeek -> String
weekToNumber Mon = "1"
weekToNumber Tue = "2"
weekToNumber Wed = "3"
weekToNumber Thu = "4"
weekToNumber Fri = "5"
weekToNumber Sat = "6"
weekToNumber Sun = "7"

data Filter = Filter {
    stopId :: Maybe Int,
    timeOfDay :: Maybe TimeOfDay,
    dayOfWeek :: Maybe DayOfWeek
} deriving Show

-- | Get the frequency distribution of the punctualities for a given bus stop
getFrequencies :: Filter -> IO [Frequency]
getFrequencies (Filter mStopId mTime mDay) = do
    conn <- open "database.db"
    print queryString
    print parameters
    result <- queryNamed conn (Query queryString) parameters :: IO [Frequency]
    close conn
    return result
    where 
        baseQuery = "SELECT punctuality, COUNT(*) AS frequency \
                            \FROM actual_arrivals "
        
        (conditions, parameters) = foldr addFilter ([], []) [
                fmap (\s -> ("stop_id = :stop_id", Text.pack ":stop_id" := s)) mStopId,
                fmap (\t -> ("time(timestamp) >= time(:time_of_day_0, '-30 minutes')", ":time_of_day_0" := show t)) mTime,
                fmap (\t -> ("time(timestamp) <= time(:time_of_day_1, '+30 minutes')", ":time_of_day_1" := show t)) mTime,
                fmap (\d -> ("strftime('%w', timestamp) = :day_of_week", ":day_of_week" := weekToNumber d)) mDay
            ]

        addFilter Nothing acc = acc
        addFilter (Just (condition, parameter)) (condition_list, parameter_list) = (condition : condition_list, parameter: parameter_list)

        intermediateQuery = if null conditions
            then baseQuery
            else Text.concat [baseQuery, " WHERE ", Text.intercalate " AND " conditions]

        queryString = intermediateQuery <> " GROUP BY punctuality"
