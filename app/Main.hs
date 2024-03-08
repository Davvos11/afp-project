module Main where

import Subscriber
import Parser
import Database

main :: IO ()
main = do
  x <- Database.getFrequenciesForBusStop 1
  mapM_ print x
  Subscriber.run
