module Msg exposing (..)

import Model exposing (Direction)
import Time

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
         | GotStops (List (String, Int))
         | GotBuses (List (String, Int))
         | GotDelays Int Int

type TimeMod = PlusHour
             | MinusHour
             | Plus15Min
             | Minus15Min
             | Plus5Min
             | Minus5Min