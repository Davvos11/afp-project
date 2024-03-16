module Model exposing (..)

import Time
import Dict

type Model = Model {departure : Stop,
                    destination : Stop,
                    bus : Bus,
                    moment : Moment,
                    delays : DelayInfo,
                    stops : Dict.Dict String (Int, String),
                    buses : Dict.Dict String (Int, String),
                    loading : LoadingState,
                    time : (Time.Posix, Time.Zone)}

type Moment = Moment {day : Time.Weekday, hour : Int, minute : Int}

toDayNumber : Time.Weekday -> Int
toDayNumber t = case t of
                  Time.Mon -> 0
                  Time.Tue -> 1
                  Time.Wed -> 2
                  Time.Thu -> 3
                  Time.Fri -> 4
                  Time.Sat -> 5
                  Time.Sun -> 6

type Stop = Stop Int (() -> String)
          | NoStop String

type Direction = Departure
               | Destination

type Bus = Bus Int (() -> String)
         | NoBus String

type LoadingState = Loading
                  | NotLoading

type alias DelayInfo = Maybe (Int, Int)