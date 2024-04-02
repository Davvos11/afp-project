module Main where

import Database
import Parser
import Server
import Subscriber ( run )
import Data.Time (DayOfWeek(Thursday))

main :: IO ()
main = do
  putStrLn "Generating line and stop id's..."
  -- TODO run this periodically?
  Database.generateLines
  Database.generateStops
  -- x <- Database.getFrequencies False $ Filter {
  --   startStop = Just 183,
  --   endStop = Just 100,
  --   linePlanningNumber = Just 45,
  --   timeOfDay = Just (TimeOfDay 17 00),
  --   dayOfWeek = Just Thursday
  -- }
  -- x <- getLines
  -- x <- getStops
  -- mapM_ print x
  -- Subscriber.run
  runServer
