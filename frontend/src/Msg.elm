module Msg exposing (..)

import Time
import Http

type Msg = -- Input changes
           BusChange String
         | StopChange String RouteEndpoint
         | DayChange Time.Weekday
         | TimeChange TimeMod
         | ReverseStops
         -- | Check that input in bus/stop field represents a valid bus/stop
         | ValidateBus
         | ValidateStop RouteEndpoint
         -- Request delay calculation
         | CalcDelay
         -- To set the default time to local time, not used afterwards
         | NewTime Time.Posix Time.Zone
         -- Http
         -- Inititalization
         | GotStops (Result Http.Error (List (String, Int)))
         | GotBuses (Result Http.Error (List (String, Int)))
         -- Delay request
         | GotDelays (Result Http.Error (Int, Int))

-- | Whether functionality involving a stop should work on the departure or destination stop
type RouteEndpoint = Departure
                   | Destination

-- | Constrained list of possible modifications to make to the time that is to be queried
type TimeMod = PlusHour
             | MinusHour
             | Plus15Min
             | Minus15Min
             | Plus5Min
             | Minus5Min

-- | Encodes weekday as int for query.
-- Week starts at Monday, index 0, and so on.
toDayNumber : Time.Weekday -> Int
toDayNumber t = case t of
                  Time.Mon -> 0
                  Time.Tue -> 1
                  Time.Wed -> 2
                  Time.Thu -> 3
                  Time.Fri -> 4
                  Time.Sat -> 5
                  Time.Sun -> 6