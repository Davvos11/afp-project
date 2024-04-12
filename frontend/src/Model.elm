module Model exposing (..)

import Time
import Dict
import List exposing (map, foldr)

type Model = Model {departure : Stop,
                    destination : Stop,
                    bus : Bus,
                    momentInWeek : MomentInWeek,
                    delays : DelayInfo,                     --^ Calculate delay data, if any
                    stops : Dict.Dict String (Int, String), --^ Key: lowercased name, Value: (id, name proper)
                    buses : Dict.Dict String (Int, String), --^ Key: lowercased name, Value: (id, name proper)
                    loading : LoadingState,
                    time : (Time.Posix, Time.Zone)          --^ Current time, only used for initialization
                   }

type MomentInWeek = MomentInWeek {day : Time.Weekday, hour : Int, minute : Int}

type Stop = Stop Int (() -> String) --^ An identified stop, storing its id and a function that will always return the same string (to display the correct name)
          | NoStop String           --^ Current value in the input field, not (yet) succesfully parsed as a stop

type Bus = Bus Int (() -> String) --^ An identified bus, storing its id and a function that will always return the same string (to display the correct name)
         | NoBus String           --^ Current value in the input field, not (yet) succesfully parsed as a bus

-- | Whether we're waiting on a response from the server.
-- Currently tracked but not used
type LoadingState = Loading
                  | NotLoading

-- | Punctualities, Frequencies + Sum of Frequencies
type alias DelayFrequencies = (List (Int, Int), Int)

delayDataToFrequencies : List (Int, Int) -> DelayFrequencies
delayDataToFrequencies ls = (ls, foldr (+) 0 (map Tuple.second ls))

-- | Sorted frequency distribution of the delays at (departure, destination), in format (punctuality, frequency)
type alias DelayInfo = Maybe (DelayFrequencies, DelayFrequencies)