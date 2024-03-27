{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Database (Filter (..), TimeOfDay (..), getFrequencies, generateLines, getLines, generateStops, getStops) where

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
    startStop :: Maybe String,
    endStop :: Maybe String,
    linePlanningNumber :: Maybe String,
    timeOfDay :: Maybe TimeOfDay,
    dayOfWeek :: Maybe DayOfWeek
} deriving Show

-- | Get the frequency distribution of the punctualities for a given bus stop
getFrequencies :: Filter -> IO [Frequency]
getFrequencies (Filter mStartStop mEndStop mLine mTime mDay) = do
    conn <- open "database-prod.db"
    print queryString
    print parameters
    result <- queryNamed conn (Query queryString) parameters :: IO [Frequency]
    close conn
    return result
    where
        baseQuery = "SELECT a1.lineplanningnumber, a1.punctuality / 60 AS punctuality_min, COUNT(*) AS frequency \
                            \FROM actual_arrivals AS a1 \
                            \JOIN stops as s1 ON s1.stop_code = a1.stop_code \
                            \WHERE a1.type = 'DEPARTURE' "

        (conditions, parameters) = foldr addFilter ([], []) [
                fmap (\s -> ("s1.name LIKE :end_stop_name", ":end_stop_name" := likeString s)) mEndStop,
                fmap (\l -> ("a1.lineplanningnumber = :line_number", ":line_number" := l)) mLine,
                fmap (\t -> ("time(a1.timestamp) >= time(:time_of_day_0, '-15 minutes')", ":time_of_day_0" := show t)) mTime,
                fmap (\t -> ("time(a1.timestamp) <= time(:time_of_day_1, '+15 minutes')", ":time_of_day_1" := show t)) mTime,
                fmap (\d -> ("strftime('%w', a1.timestamp) = :day_of_week", ":day_of_week" := weekToNumber d)) mDay,
                fmap (\s ->
                    ("EXISTS (\
                        \SELECT 1 \
                        \FROM actual_arrivals AS a2 \
                        \JOIN stops AS s2 ON s2.stop_code = a2.stop_code \
                        \WHERE a2.journey_id = a1.journey_id \
                        \AND a2.timestamp < a1.timestamp \
                        \AND s2.name LIKE :start_stop_name \
                    \)",
                    ":start_stop_name" := likeString s)
                ) mStartStop
            ]

        addFilter Nothing acc = acc
        addFilter (Just (condition, parameter)) (condition_list, parameter_list) = (condition : condition_list, parameter: parameter_list)

        intermediateQuery = if null conditions
            then baseQuery
            else Text.concat [baseQuery, " AND ", Text.intercalate " AND " conditions]

        queryString = intermediateQuery <> " GROUP BY punctuality_min ORDER BY frequency DESC"

-- | Get an enumerated list of bus lines, from the `lines` db table.
getLines :: IO [(String, Int)]
getLines = do
    conn <- open "database-prod.db"
    result <- query_ conn queryString :: IO [(String, Int)]
    close conn
    return result
    where
        queryString = "SELECT lineplanningnumber, id FROM lines"

-- | Generate a list of bus lines and save it to the `lines` db table.
generateLines :: IO ()
generateLines = do
    conn <- open "database-prod.db"
    execute_ conn createTable
    execute_ conn insertLines
    close conn
    where
        createTable = "CREATE TABLE IF NOT EXISTS \"lines\" (\
	                        \ \"id\" INTEGER, \
	                        \ \"lineplanningnumber\" TEXT UNIQUE, \
	                        \PRIMARY KEY(\"id\" AUTOINCREMENT) \
                            \);"
        insertLines = "INSERT INTO lines (lineplanningnumber) \
                        \SELECT DISTINCT lineplanningnumber FROM actual_arrivals;"
