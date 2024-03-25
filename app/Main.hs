module Main where

import Database
import Parser
import Server
import Subscriber

main :: IO ()
main = do
  x <- Database.getFrequencies $ Filter {
    startStop = Just "Rijnsweerd Zuid",
    endStop = Just "Kanaleneiland",
    linePlanningNumber = Just "u034",
    timeOfDay = Just (TimeOfDay 17 00),
    dayOfWeek = Nothing
  }
  mapM_ print x
  Subscriber.run
  runServer
