module ExampleData (stopData, busData, delayData) where

import MessageFormat

exampleStops :: [(String, Int)]
exampleStops = [("P+R Science Park", 0),
                ("WKZ / Máxima", 1),
                ("UMC Utrecht", 2),
                ("Heidelberglaan", 3),
                ("Botanische Tuinen", 4),
                ("Rijnsweerd-Zuid", 5),
                ("Rijnsweerd-Noord", 6)
                ]

stopData :: Stops
stopData = Stops $ map toStopPair exampleStops

exampleBuses :: [(String, Int)]
exampleBuses = [("28", 0),
                ("202", 1),
                ("200", 2),
                ("30", 3),
                ("34", 5)
               ]

busData :: Buses
busData = Buses $ map toBusPair exampleBuses

delayData :: Delays
delayData = Delays { departureDelays   = [DelayFrequency (-3) 1, DelayFrequency (-1) 3, DelayFrequency 0 8, DelayFrequency 1 4, DelayFrequency 3 3]
                   , destinationDelays = [DelayFrequency (-2) 1, DelayFrequency 0 2, DelayFrequency 4 3, DelayFrequency 5 10, DelayFrequency 6 3]}