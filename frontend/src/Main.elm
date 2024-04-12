module Main exposing (..)

import Browser
import Html exposing (text, div, Html, button, input, datalist, select, option)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, placeholder, id, list)
import Http
import Url.Builder
import Json.Decode exposing (field, string, int)
import Task

import Time exposing (utc, toHour, toMinute, toWeekday)
import Dict
import List
import String

import Model exposing (..)
import Msg exposing (..)

import Chart.Bar as Chart

backend_url = "http://localhost:3000"
-- backend_url = "https://delay-forecast-api.dovatvis.nl"

main : Program () Model Msg
main = Browser.element {init = init,
                        update = update,
                        subscriptions = subscriptions,
                        view = view}

-- | Initializes model and creates request (that will chain into other requests) for dynamic initial data
init : () -> (Model, Cmd Msg)
init _ = (Model {departure    = NoStop "",
                 destination  = NoStop "",
                 bus          = NoBus "",
                 momentInWeek = MomentInWeek {day = Time.Mon, hour = 0, minute = 0},
                 delays       = Nothing,
                 stops        = Dict.empty,
                 buses        = Dict.empty,
                 loading      = NotLoading,
                 time         = (Time.millisToPosix 0, utc)
                },
          Task.perform identity (Task.map2 NewTime Time.now Time.here))

-- | Takes a list of paired names/ids, and turns them into dicts for looking up the proper name/ids based on the lowercase name
toDict : List (String, Int) -> Dict.Dict String (Int, String)
toDict xs = Dict.fromList (List.map (\(x, i) -> (String.toLower x, (i, x))) xs)

-- | Updates the model
update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model m) = case msg of
    -- Initialization
    NewTime t z     -> (Model {m | time = (t, z), momentInWeek = MomentInWeek {day = toWeekday z t, hour = toHour z t, minute = toMinute z t}}, requestStops)
    GotStops res    -> case res of
                         Ok stops -> (Model {m | stops = toDict stops}, requestBuses)
                         Err _    -> (Model m, Cmd.none)
    GotBuses res    -> case res of
                         Ok buses -> (Model {m | buses = toDict buses}, Cmd.none)
                         Err _    -> (Model m, Cmd.none)
    TimeChange t    -> (Model {m | momentInWeek = (case t of
                                                     Minus5Min  -> subMinute m.momentInWeek 5
                                                     Minus15Min -> subMinute m.momentInWeek 15
                                                     MinusHour  -> subMinute m.momentInWeek 60
                                                     Plus5Min   -> addMinute m.momentInWeek 5
                                                     Plus15Min  -> addMinute m.momentInWeek 15
                                                     PlusHour   -> addMinute m.momentInWeek 60
                        )}, Cmd.none)
    DayChange d     -> (Model {m | momentInWeek = (\(MomentInWeek momentInWeek) -> MomentInWeek {momentInWeek | day = d}) m.momentInWeek}, Cmd.none)
    StopChange x d  -> (Model (case d of
                                -- Stop changed, needs to be revalidated
                                Departure   -> {m | departure   = tryParseStop (NoStop x) m.stops}
                                Destination -> {m | destination = tryParseStop (NoStop x) m.stops})
                             , Cmd.none)
                                -- Same for the bus
    BusChange x     -> (Model {m | bus = tryParseBus (NoBus x) (m.buses)}, Cmd.none)
    ReverseStops    -> (Model {m | departure = m.destination, destination = m.departure}, Cmd.none)
    CalcDelay       -> (Model {m | loading = Loading}, requestDelays (Model m))
    GotDelays res   -> case res of
                         Ok (d1, d2) -> (Model {m | delays = Just (delayDataToFrequencies d1, delayDataToFrequencies d2), loading = NotLoading}, Cmd.none)
                         Err _       -> (Model {m | loading = NotLoading}, Cmd.none)

-- | Attempts to find the name in a bus field in its known dict of buses to create an identified bus
tryParseBus : Bus -> Dict.Dict String (Int, String) -> Bus
tryParseBus b dict = case b of
    Bus i f -> Bus i f
    NoBus x -> case Dict.get (String.toLower x) dict of
               Nothing          -> NoBus x
               Just (i, xprime) -> Bus (always i) (always xprime)

-- | Attempts to find the name in a stop field in its known dict of stops to create an identified stop
tryParseStop : Stop -> Dict.Dict String (Int, String) -> Stop
tryParseStop s dict = case s of
    Stop i f -> Stop i f
    NoStop x -> case Dict.get (String.toLower x) dict of
               Nothing          -> NoStop x
               Just (i, xprime) -> Stop (always i) (always xprime)

-- | Creates the request to calculate the delays for this bus and these stops
-- Idea to consider and discuss: don't send request if departure/destination are equal, or keep as feature instead to poll only a single stop
requestDelays : Model -> Cmd Msg
requestDelays (Model m) = case (m.departure, m.destination, m.bus) of
  (Stop i1 _, Stop i2 _, Bus i3 _) -> Http.request
    { method = "GET",
      headers = [],
      url = Url.Builder.crossOrigin backend_url ["delays"] [Url.Builder.int "departure"   (i1 ()),
                                                            Url.Builder.int "destination" (i2 ()),
                                                            Url.Builder.int "bus"         (i3 ()),
                                                            Url.Builder.int "day"    ((\(MomentInWeek momentInWeek) -> toDayNumber momentInWeek.day)    m.momentInWeek),
                                                            Url.Builder.int "hour"   ((\(MomentInWeek momentInWeek) ->             momentInWeek.hour)   m.momentInWeek),
                                                            Url.Builder.int "minute" ((\(MomentInWeek momentInWeek) ->             momentInWeek.minute) m.momentInWeek)
                                                           ],
      body = Http.emptyBody,
      expect = Http.expectJson GotDelays
      (Json.Decode.map2 (\a b -> (a, b)) (field "departureDelays" (Json.Decode.list decodeFrequency)) (field "destinationDelays" (Json.Decode.list decodeFrequency))),
      timeout = Nothing,
      tracker = Nothing
    }
  (_, _, _) -> Cmd.none

decodeFrequency : Json.Decode.Decoder (Int, Int)
decodeFrequency = Json.Decode.map2 (\a b -> (a, b)) (field "punct" int) (field "freq" int)

-- | Factory method for the static buses/stops requests
requestStatic : String -> String -> ((Result Http.Error (List (String, Int))) -> Msg) -> Cmd Msg
requestStatic singular plural msgToSend = Http.request { method = "GET",
                                                         headers = [],
                                                         url = Url.Builder.crossOrigin backend_url [plural] [],
                                                         body = Http.emptyBody,
                                                         expect = Http.expectJson msgToSend (field plural (Json.Decode.list (Json.Decode.map2 (\a b -> (a, b)) (field (singular ++ "Name") string) (field (singular ++ "Id") int)))),
                                                         timeout = Nothing,
                                                         tracker = Nothing
                                                       }
requestStops : Cmd Msg
requestStops = requestStatic "stop" "stops" GotStops

requestBuses : Cmd Msg
requestBuses = requestStatic "bus" "buses" GotBuses

-- | How to display the model
-- Massive todo, turn this into smaller, digestable bits
view : Model -> Html Msg
view (Model m) = div [] ([ -- Time changes, display in between - & +
                         button [onClick (TimeChange Minus5Min)] [text "-5"],
                         button [onClick (TimeChange Minus15Min)] [text "-15"],
                         button [onClick (TimeChange MinusHour)] [text "-60"],
                         text ((\(MomentInWeek momentInWeek) -> (if momentInWeek.hour < 10 then "0" else "") ++ String.fromInt momentInWeek.hour ++ ":" ++ (if momentInWeek.minute < 10 then "0" else "") ++ String.fromInt momentInWeek.minute) m.momentInWeek),
                         button [onClick (TimeChange PlusHour)] [text "+60"],
                         button [onClick (TimeChange Plus15Min)] [text "+15"],
                         button [onClick (TimeChange Plus5Min)] [text "+5"],
                         -- Day selection
                         div [] [button [onClick (DayChange Time.Mon)] [text "Ma"],
                                 button [onClick (DayChange Time.Tue)] [text "Di"],
                                 button [onClick (DayChange Time.Wed)] [text "Wo"],
                                 button [onClick (DayChange Time.Thu)] [text "Do"],
                                 button [onClick (DayChange Time.Fri)] [text "Vr"],
                                 button [onClick (DayChange Time.Sat)] [text "Za"],
                                 button [onClick (DayChange Time.Sun)] [text "Zo"],
                                 text (toDutchWeekday ((\(MomentInWeek momentInWeek) -> momentInWeek.day) m.momentInWeek))],
                        -- Stop input
                         div [] ([input [placeholder "Vertrekhalte",
                                         value (case m.departure of
                                                  Stop i f -> f ()
                                                  NoStop x -> x),
                                         Html.Attributes.list "stops",
                                         onInput (\x -> StopChange x Departure)] [],
                                  datalist [id "stops"] [
                                    select [] (
                                      Dict.values m.stops |> List.map (\(k, v) -> option [] [text v])
                                    )
                                  ]] ++ (case m.departure of
                                           NoStop x -> []
                                           Stop i f -> [text "Geldige halte"])),
                         div [] [button [onClick ReverseStops] [text "⇅"]],
                         div [] ([input [placeholder "Aankomsthalte",
                                         Html.Attributes.list "stops",
                                         value (case m.destination of
                                                  Stop i f -> f ()
                                                  NoStop x -> x),
                                         onInput (\x -> StopChange x Destination)] []
                                  ] ++ (case m.destination of
                                          NoStop x -> []
                                          Stop i f -> [text "Geldige halte"])),
                         -- Bus input
                         div [] ([input [placeholder "Bus",
                                         Html.Attributes.list "buses",
                                         value (case m.bus of
                                                  Bus i f -> f ()
                                                  NoBus x -> x),
                                         onInput (\x -> BusChange x)] [],
                                  datalist [id "buses"] [
                                    select [] (
                                      Dict.values m.buses |> List.map (\(k, v) -> option [] [text v])
                                    )
                                  ]] ++ (case m.bus of
                                           Bus i f -> [text "Geldige bus"]
                                           NoBus x -> [])),
                         div [] [button [onClick CalcDelay] [text "Voorspel vertraging"]]
                ] ++ (case m.departure of
                        NoStop x -> []
                        Stop i f -> case m.delays of
                                      Nothing       -> []
                                      Just (d1, d2) -> [plot d1 (f ())])
                ++ (case m.destination of
                        NoStop x -> []
                        Stop i f -> case m.delays of
                                      Nothing       -> []
                                      Just (d1, d2) -> [plot d2 (f ())])
                )
-- | Plots the frequency distribution of delays at the given stop
plot : DelayFrequencies -> String -> Html Msg
plot (ls, tot) n = div [] [text ("Vertraging bij halte " ++ n ++ ":"),
                           Chart.render (ls, {xGroup = always Nothing,
                                              xValue = (\(p, _) -> String.fromInt p ++ " min"),
                                              yValue = (\(_, f) -> toFloat f / toFloat tot * 100)})
                                        ((Chart.init { margin =
                                                          { top = 10
                                                          , right = 10
                                                          , bottom = 30
                                                          , left = 30
                                                          }
                                                      , width = 500
                                                      , height = 200
                                                      }) {- |> Can add more configuration here -})
                           ]

-- | Subtracts up to 60 minutes from a given MomentInWeek. Does not change 'day' on wrap-around
subMinute : MomentInWeek -> Int -> MomentInWeek
subMinute (MomentInWeek m) x = MomentInWeek {m | hour=if m.minute - x < 0 then (if m.hour - 1 < 0 then m.hour + 23 else m.hour - 1) else m.hour, minute=if m.minute - x < 0 then m.minute - x + 60 else m.minute - x}

-- | Adds up to 60 minutes to a given MomentInWeek. Does not change 'day' on wrap-around.
addMinute : MomentInWeek -> Int -> MomentInWeek
addMinute (MomentInWeek m) x = MomentInWeek {m | hour=if m.minute + x > 59 then (if m.hour + 1 > 23 then m.hour - 23 else m.hour + 1) else m.hour, minute=if m.minute + x > 59 then m.minute + x - 60 else m.minute + x}

-- | Creates a string representation of a weekday, in Dutch
toDutchWeekday : Time.Weekday -> String
toDutchWeekday t = (case t of
                      Time.Mon -> "Maan"
                      Time.Tue -> "Dins"
                      Time.Wed -> "Woens"
                      Time.Thu -> "Donder"
                      Time.Fri -> "Vrij"
                      Time.Sat -> "Zater"
                      Time.Sun -> "Zon")
                    ++ "dag"

-- | Events to poll all the occasionally / frequently, without user input
-- Idea to consider: do this with stops/bus parsing or requesting the stop / bus data again if these are still empty
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
