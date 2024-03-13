module Msg exposing (..)

import Model exposing (Direction)
import Time
import Http

type Msg = BusChange String
         | StopChange String Direction
         | ValidateBus
         | ValidateStop Direction
         | DayChange Time.Weekday
         | TimeChange TimeMod
         | CalcDelay
         | ReverseStops
         -- To set the default time to local time
         | NewTime Time.Posix Time.Zone
         -- Http
         | GotStops (Result Http.Error (List (String, Int)))
         | GotBuses (Result Http.Error (List (String, Int)))
         | GotDelays (Result Http.Error (Int, Int))

type TimeMod = PlusHour
             | MinusHour
             | Plus15Min
             | Minus15Min
             | Plus5Min
             | Minus5Min