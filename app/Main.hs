module Main where

import Parser
import Database
import Parser
import Server
import Data.Time (DayOfWeek(Thursday))

main :: IO ()
main = do
  putStrLn "Generating line and stop id's..."
  -- TODO run this periodically?
  Database.generateLines
  Database.generateStops
  runServer
