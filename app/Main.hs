module Main where

import Database
import Parser
import Server
import Subscriber

main :: IO ()
main = do
  x <- Database.getFrequencies $ Filter {
    stopName = Just "Utrecht, Kanaleneiland",
    linePlanningNumber = Just "u085",
    timeOfDay = Just (TimeOfDay 21 15),
    dayOfWeek = Nothing
  }
  mapM_ print x
  Subscriber.run
  runServer
