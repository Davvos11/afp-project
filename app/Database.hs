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

likeString :: String -> String
likeString s = "%" ++ s ++ "%"

data Filter = Filter {
    stopName :: Maybe String,
    linePlanningNumber :: Maybe String,
    timeOfDay :: Maybe TimeOfDay,
    dayOfWeek :: Maybe DayOfWeek
} deriving Show

-- | Get the frequency distribution of the punctualities for a given bus stop
getFrequencies :: Filter -> IO [Frequency]
getFrequencies (Filter mStop mLine mTime mDay) = do
    conn <- open "database-prod.db"
    print queryString
    print parameters
    result <- queryNamed conn (Query queryString) parameters :: IO [Frequency]
    close conn
    return result
    where 
        baseQuery = "SELECT a.lineplanningnumber, a.punctuality / 60 AS punctuality_min, COUNT(*) AS frequency \
                            \FROM actual_arrivals AS a \
                            \JOIN stops as s ON s.stop_code = a.stop_code \
                            \WHERE a.type = 'DEPARTURE' "
        
        (conditions, parameters) = foldr addFilter ([], []) [
                fmap (\s -> ("s.name LIKE :stop_name", ":stop_name" := likeString s)) mStop,
                fmap (\l -> ("a.lineplanningnumber = :line_number", ":line_number" := l)) mLine,
                fmap (\t -> ("time(timestamp) >= time(:time_of_day_0, '-15 minutes')", ":time_of_day_0" := show t)) mTime,
                fmap (\t -> ("time(timestamp) <= time(:time_of_day_1, '+15 minutes')", ":time_of_day_1" := show t)) mTime,
                fmap (\d -> ("strftime('%w', timestamp) = :day_of_week", ":day_of_week" := weekToNumber d)) mDay
            ]

        addFilter Nothing acc = acc
        addFilter (Just (condition, parameter)) (condition_list, parameter_list) = (condition : condition_list, parameter: parameter_list)

        intermediateQuery = if null conditions
            then baseQuery
            else Text.concat [baseQuery, " AND ", Text.intercalate " AND " conditions]

        queryString = intermediateQuery <> " GROUP BY punctuality_min ORDER BY frequency DESC"
