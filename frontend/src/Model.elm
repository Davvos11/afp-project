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

type Stop = Stop Int (() -> String)
          | NoStop String

type Direction = Departure
               | Destination

type Bus = Bus Int (() -> String)
         | NoBus String

type LoadingState = Loading
                  | NotLoading

type alias DelayInfo = Maybe (Int, Int)