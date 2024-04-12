{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Database (Filter (..), TimeOfDay (..), getFrequencies, generateLines, getLines, generateStops, getStops) where

import Database.Models
import Database.SQLite.Simple
import qualified Data.Text as Text
import Text.Printf
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Text.Read (readMaybe)
import Data.Time (DayOfWeek)

data TimeOfDay = TimeOfDay { hours :: Int, minutes :: Int }

instance Show TimeOfDay where
    show :: TimeOfDay -> String
    show (TimeOfDay h m) = printf "%02d" h ++ ":" ++ printf "%02d" m ++ ":00"

weekToNumber :: DayOfWeek -> String
weekToNumber = show . fromEnum

data Filter = Filter {
    startStop :: Maybe Int,
    endStop :: Maybe Int,
    linePlanningNumber :: Maybe Int,
    timeOfDay :: Maybe TimeOfDay,
    dayOfWeek :: Maybe DayOfWeek
} deriving Show

-- | Get the frequency distribution of the punctualities for a given bus stop
-- The boolean `get_start` changes the behaviour of the method to get the delay at the start stop,
-- instead of at the end_stop.
getFrequencies :: Bool -> Filter -> IO [Frequency]
getFrequencies get_start f@(Filter mStartStop mEndStop mLine mTime mDay) = do
    conn <- open "database-prod.db"
    print f
    print queryString
    print parameters
    result <- queryNamed conn (Query queryString) parameters :: IO [Frequency]
    close conn
    return result
    where
        baseQuery = "SELECT a1.lineplanningnumber, a1.punctuality / 60 AS punctuality_min, COUNT(*) AS frequency \
                            \FROM actual_arrivals AS a1 \
                            \JOIN stop_names as s1 ON s1.stop_code = a1.stop_code \
                            \JOIN lines as l1 ON l1.lineplanningnumber = a1.lineplanningnumber \
                            \WHERE a1.type = 'DEPARTURE' "

        (conditions, parameters) = foldr addFilter ([], []) [
                fmap (\i -> ("s1.frontend_id = :end_stop_id", ":end_stop_id" := i)) (if get_start then mStartStop else mEndStop),
                fmap (\l -> ("l1.id = :line_id", ":line_id" := l)) mLine,
                fmap (\t -> ("time(a1.timestamp) >= time(:time_of_day_0, '-30 minutes')", ":time_of_day_0" := show t)) mTime,
                fmap (\t -> ("time(a1.timestamp) <= time(:time_of_day_1, '+30 minutes')", ":time_of_day_1" := show t)) mTime,
                fmap (\d -> ("strftime('%w', a1.timestamp) = :day_of_week", ":day_of_week" := weekToNumber d)) mDay,
                fmap (\i ->
                    (Text.concat ["EXISTS (\
                        \SELECT 1 \
                        \FROM actual_arrivals AS a2 \
                        \JOIN stop_names AS s2 ON s2.stop_code = a2.stop_code \
                        \WHERE a2.journey_id = a1.journey_id \
                        \AND a2.timestamp ", if get_start then ">" else "<" ," a1.timestamp \
                        \AND s2.frontend_id = :start_stop_id \
                    \)"],
                    ":start_stop_id" := i)
                ) (if get_start then mEndStop else mStartStop)
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
    execute_ conn dropTable
    execute_ conn createTable
    execute_ conn insertLines
    close conn
    where
        dropTable = "DROP TABLE IF EXISTS \"lines\""
        createTable = "CREATE TABLE IF NOT EXISTS \"lines\" (\
	                        \ \"id\" INTEGER, \
	                        \ \"lineplanningnumber\" TEXT UNIQUE, \
	                        \PRIMARY KEY(\"id\" AUTOINCREMENT) \
                            \);"
        insertLines = "INSERT INTO lines (lineplanningnumber) \
                        \SELECT DISTINCT lineplanningnumber FROM actual_arrivals;"

-- | Get an enumerated list of bus stops, from the `stops` db table.
getStops :: IO [(String, Int)]
getStops = do
    conn <- open "database-prod.db"
    result <- query_ conn queryString :: IO [(String, Int)]
    close conn
    return result
    where
        queryString = "SELECT DISTINCT name, frontend_id FROM stop_names"

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

cleanupStopName :: String -> String
cleanupStopName = trim . takeWhile (/= '(')

-- | Process stops, input is a list of (name, stop_code)
processStops :: [(String, String)] -> Map String [Int]
processStops = foldl' insertClean Map.empty
    where
        insertClean acc (name, stop_id) =
            case readMaybe stop_id of
                Just stop_id_int -> let key = cleanupStopName name
                           in Map.insertWith (++) key [stop_id_int] acc
                Nothing -> acc

mapListToEnumTupleList :: Map a [b] -> [(Int, a, b)]
mapListToEnumTupleList = fst . Map.foldrWithKey convert ([], 0)
    where 
        convert key values (acc, i) = (acc ++ map (\v -> (i, key, v)) values, i + 1)


-- | Generate a list of bus stops and save it to the `stops` db table.
generateStops :: IO ()
generateStops = do
    conn <- open "database-prod.db"
    execute_ conn dropTable
    execute_ conn createTable
    stops <- query_ conn getNames :: IO [(String, String)]
    let disinct_stops = mapListToEnumTupleList $ processStops stops
    -- print disinct_stops
    executeMany conn insertStops disinct_stops
    close conn
    where
        dropTable = "DROP TABLE IF EXISTS \"stop_names\""
        createTable = "CREATE TABLE IF NOT EXISTS \"stop_names\" (\
	                        \ \"id\" INTEGER, \
                            \ \"frontend_id\" INTEGER, \
	                        \ \"name\" TEXT, \
                            \ \"stop_code\" Integer, \
	                        \PRIMARY KEY(\"id\" AUTOINCREMENT) \
                            \UNIQUE(\"stop_code\", \"name\") \
                            \);"
        getNames = "SELECT DISTINCT name, stop_code FROM stops"
        insertStops = "INSERT INTO stop_names (frontend_id, name, stop_code) VALUES (?, ?, ?)"


