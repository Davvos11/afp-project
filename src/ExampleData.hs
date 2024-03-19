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
delayData = Delays {departureDelay = 2, destinationDelay = 4}