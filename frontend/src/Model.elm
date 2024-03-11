module Model exposing (..)

import Time
import Dict

type alias Model = {departure : Stop,
                    destination : Stop,
                    bus : Bus,
                    moment : {day : Time.Weekday, hour : Int, minute : Int},
                    delays : DelayInfo,
                    stops : Dict.Dict String (Int, String),
                    buses : Dict.Dict String (Int, String),
                    loading : LoadingState,
                    time : (Time.Posix, Time.Zone)}

type Stop = Stop Int
          | NoStop String

type Direction = Departure
               | Destination

type Bus = Bus Int
         | NoBus String

type LoadingState = Loading
                  | NotLoading

type alias DelayInfo = Maybe (Int, Int)