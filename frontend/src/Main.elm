module Main exposing (..)

import Browser
import Html exposing (text, div, Html, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, placeholder)
import Http
import Json.Decode exposing (field, string, int, list)
import Task

import Time exposing (utc, toHour, toMinute, toWeekday)
import Dict
import List
import String

import Model exposing (..)
import Msg exposing (..)
import StopBusExample exposing (..)

main : Program () Model Msg
main = Browser.element {init = init,
                        update = update,
                        subscriptions = subscriptions,
                        view = view}

init : () -> (Model, Cmd Msg)
init _ = (Model {departure   = NoStop "",
                 destination = NoStop "",
                 bus         = NoBus "",
                 moment      = Moment {day = Time.Mon, hour = 0, minute = 0},
                 delays      = Nothing,
                 stops       = Dict.empty,
                 buses       = Dict.empty,
                 loading     = NotLoading,
                 time        = (Time.millisToPosix 0, utc)
                },
          Task.perform identity (Task.map2 NewTime Time.now Time.here))


toDict : List (String, Int) -> Dict.Dict String (Int, String)
toDict xs = Dict.fromList (List.map (\(x, i) -> (String.toLower x, (i, x))) xs)

update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model m) = case msg of
    ReverseStops    -> (Model {m | departure = m.destination, destination = m.departure}, Cmd.none)
    NewTime t z     -> (Model {m | time = (t, z), moment = Moment {day = toWeekday z t, hour = toHour z t, minute = toMinute z t}}, requestStops)
    TimeChange t    -> (Model {m | moment = (case t of
                                               Minus5Min  -> subMinute m.moment 5
                                               Minus15Min -> subMinute m.moment 15
                                               MinusHour  -> subMinute m.moment 60
                                               Plus5Min   -> addMinute m.moment 5
                                               Plus15Min  -> addMinute m.moment 15
                                               PlusHour   -> addMinute m.moment 60
                        )}, Cmd.none)
    DayChange d     -> (Model {m | moment = (\(Moment moment) -> Moment {moment | day = d}) m.moment}, Cmd.none)
    StopChange x d  -> (Model (case d of
                           -- Halte changed, needs to be revalidated
                           -- Todo reset delays when these change
                           Departure   -> {m | departure   = NoStop x}
                           Destination -> {m | destination = NoStop x}
                      ), Cmd.none)
                           -- Same for the bus
    BusChange x     -> (Model {m | bus = NoBus x}, Cmd.none)
    ValidateBus     -> (Model {m | bus = tryParseBus (m.bus) (m.buses)}, Cmd.none)
    ValidateStop d  -> (Model (case d of
                           Departure   -> {m | departure   = tryParseStop m.departure   m.stops}
                           Destination -> {m | destination = tryParseStop m.destination m.stops}
                        ), Cmd.none)
    CalcDelay       -> (Model {m | loading = Loading}, requestDelays (Model m))
    GotStops res    -> case res of
                         Ok stops -> (Model {m | stops = toDict stops}, requestBuses)
                         Err _    -> (Model m, Cmd.none)
    GotBuses res    -> case res of
                         Ok buses -> (Model {m | buses = toDict buses}, Cmd.none)
                         Err _    -> (Model m, Cmd.none)
    GotDelays res   -> case res of
                         Ok (d1, d2) -> (Model {m | delays = Just (d1, d2), loading = NotLoading}, Cmd.none)
                         Err _       -> (Model {m | loading = NotLoading}, Cmd.none)

tryParseBus : Bus -> Dict.Dict String (Int, String) -> Bus
tryParseBus b dict = case b of
    Bus i f -> Bus i f
    NoBus x -> case Dict.get (String.toLower x) dict of
               Nothing          -> NoBus x
               Just (i, xprime) -> Bus i (always xprime)

tryParseStop : Stop -> Dict.Dict String (Int, String) -> Stop
tryParseStop s dict = case s of
    Stop i f -> Stop i f
    NoStop x -> case Dict.get (String.toLower x) dict of
               Nothing          -> NoStop x
               Just (i, xprime) -> Stop i (always xprime)

requestDelays : Model -> Cmd Msg
-- Todo: Request delays using model data
requestDelays m = Cmd.none

requestStops : Cmd Msg
-- Todo: http request the stops for demo hardcode some examples
requestStops = Http.request { method = "GET",
                              headers = [],
                              url = "http://localhost:3000/stops",
                              body = Http.emptyBody,
                              expect = Http.expectJson GotStops (field "stops" (list (Json.Decode.map2 (\a b -> (a, b)) (field "stopName" string) (field "stopId" int)))),
                              timeout = Nothing,
                              tracker = Nothing
                            }

requestBuses : Cmd Msg
-- Todo: http request the buses, for demo hardcode some examples
requestBuses = Cmd.none

view : Model -> Html Msg
view (Model m) = div [] [button [onClick (TimeChange Minus5Min)] [text "-5"],
                         button [onClick (TimeChange Minus15Min)] [text "-15"],
                         button [onClick (TimeChange MinusHour)] [text "-60"],
                         text ((\(Moment moment) -> (if moment.hour < 10 then "0" else "") ++ String.fromInt moment.hour ++ ":" ++ (if moment.minute < 10 then "0" else "") ++ String.fromInt moment.minute) m.moment),
                         button [onClick (TimeChange PlusHour)] [text "+60"],
                         button [onClick (TimeChange Plus15Min)] [text "+15"],
                         button [onClick (TimeChange Plus5Min)] [text "+5"],
                         div [] [button [onClick (DayChange Time.Mon)] [text "Ma"],
                                 button [onClick (DayChange Time.Tue)] [text "Di"],
                                 button [onClick (DayChange Time.Wed)] [text "Wo"],
                                 button [onClick (DayChange Time.Thu)] [text "Do"],
                                 button [onClick (DayChange Time.Fri)] [text "Vr"],
                                 button [onClick (DayChange Time.Sat)] [text "Za"],
                                 button [onClick (DayChange Time.Sun)] [text "Zo"],
                                 text (toDutchWeekday ((\(Moment moment) -> moment.day) m.moment))],
                         div [] ([input [placeholder "Vertrekhalte",
                                         value (case m.departure of
                                                  Stop i f -> f ()
                                                  NoStop x -> x),
                                         onInput (\x -> StopChange x Departure)] [],
                                  button [onClick (ValidateStop Departure)] [text "Check"]] ++ (case m.departure of
                                                                                                  NoStop x -> []
                                                                                                  Stop i f -> case m.delays of
                                                                                                                Nothing       -> ([text "Geldige halte"])
                                                                                                                Just (d1, d2) -> [text (if d1 == 0 then "Op tijd!" else ("+" ++ (String.fromInt d1)))])),
                         div [] [button [onClick ReverseStops] [text "⇅"]],
                         div [] ([input [placeholder "Aankomsthalte",
                                         value (case m.destination of
                                                  Stop i f -> f ()
                                                  NoStop x -> x),
                                         onInput (\x -> StopChange x Destination)] [],
                                  button [onClick (ValidateStop Destination)] [text "Check"]] ++ (case m.destination of
                                                                                                  NoStop x -> []
                                                                                                  Stop i f -> case m.delays of
                                                                                                                Nothing       -> ([text "Geldige halte"])
                                                                                                                Just (d1, d2) -> [text (if d2 == 0 then "Op tijd!" else ("+" ++ (String.fromInt d2)))])),
                         div [] ([input [placeholder "Bus",
                                         value (case m.bus of
                                                  Bus i f -> f ()
                                                  NoBus x -> x),
                                         onInput (\x -> BusChange x)] [],
                                  button [onClick ValidateBus] [text "Check"]] ++ (case m.bus of
                                                                                                  Bus i f -> [text "Geldige bus"]
                                                                                                  NoBus x -> [])),
                         div [] [button [onClick CalcDelay] [text "Voorspel vertraging"]]
                ]

subMinute : Moment -> Int -> Moment
subMinute (Moment m) x = Moment {m | hour=if m.minute - x < 0 then (if m.hour - 1 < 0 then m.hour + 23 else m.hour - 1) else m.hour, minute=if m.minute - x < 0 then m.minute - x + 60 else m.minute - x}

addMinute : Moment -> Int -> Moment
addMinute (Moment m) x = Moment {m | hour=if m.minute + x > 59 then (if m.hour + 1 > 23 then m.hour - 23 else m.hour + 1) else m.hour, minute=if m.minute + x > 59 then m.minute + x - 60 else m.minute + x}

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

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none