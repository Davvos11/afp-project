module Main where

import Subscriber
import Parser
import Database

main :: IO ()
main = do
  Subscriber.run
