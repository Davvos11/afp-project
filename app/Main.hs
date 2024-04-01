module Main where

import Parser
import Database
import Server

main :: IO ()
main = do
  x <- Database.getFrequenciesForBusStop 1
  mapM_ print x
  runServer
