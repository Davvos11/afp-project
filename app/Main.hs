module Main where

import Subscriber
import Parser
import Database

main :: IO ()
main = do
  x <- Database.getFrequencies $ Filter {stopId = Nothing, timeOfDay = Just TimeOfDay {hours = 13, minutes = 0}}
  mapM_ print x
  Subscriber.run
