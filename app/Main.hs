module Main where

import Database
import Parser
import Server
import Subscriber

main :: IO ()
main = do
  x <- Database.getFrequencies $ Filter {
    stopName = Just "Utrecht, CS Jaarbeurszijde",
    linePlanningNumber = Just "u085",
    timeOfDay = Just (TimeOfDay 17 00),
    dayOfWeek = Nothing
  }
  mapM_ print x
  Subscriber.run
  runServer
