module Main where

import Database
import Server
import System.Environment
import Control.Monad


main :: IO ()
main = do
  args <- getArgs -- Command line arguments
  unless ("skip-generate" `elem` args) $ do
      putStrLn "Generating line and stop id's..."
      -- TODO run this periodically?
      Database.generateLines
      Database.generateStops
  runServer
