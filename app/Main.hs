module Main where

import Database
import Server

main :: IO ()
main = do
  putStrLn "Generating line and stop id's..."
  -- TODO run this periodically?
  Database.generateLines
  Database.generateStops
  runServer
