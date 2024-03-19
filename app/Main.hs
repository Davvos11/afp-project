module Main where

import Database
import Parser
import Server
import Subscriber

main :: IO ()
main = do
  x <- Database.getFrequencies $ Filter {
    stopId = Nothing,
    timeOfDay = Nothing,
    dayOfWeek = Just Thu
  }
  mapM_ print x
  Subscriber.run
  runServer
