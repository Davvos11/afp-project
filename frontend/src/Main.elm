module Main exposing (..)

import Browser
import Html exposing (text, div, Html, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, placeholder)
import Http
import Time exposing (utc, toHour, toMinute, toWeekday)
import Task
import Dict
import Model exposing (..)
import Msg exposing (..)

main : Program () Model Msg
main = Browser.element {init = init,
                        update = update,
                        subscriptions = subscriptions,
                        view = view}

init : () -> (Model, Cmd Msg)
init _ = ({departure = NoStop "",
           destination = NoStop "",
           bus = NoBus "",
           moment = {day=Time.Mon, hour = 0, minute = 0},
           delays = Nothing,
           stops = Dict.empty,
           buses = Dict.empty,
           loading = NotLoading,
           time = (Time.millisToPosix 0, utc)},
          Task.perform identity (Task.map2 NewTime Time.now Time.here))

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
    ReverseStops   -> ({m | departure=m.destination, destination=m.departure}, Cmd.none)
    NewTime t z    -> ({m | time = (t, z), moment={day=toWeekday z t, hour=toHour z t, minute=toMinute z t}}, requestStops)
    TimeChange t   -> case t of
                      Minus5Min  -> ({m | moment=subMinute m.moment 5}, Cmd.none)
                      Minus15Min -> ({m | moment=subMinute m.moment 15}, Cmd.none)
                      MinusHour  -> ({m | moment=subMinute m.moment 60}, Cmd.none)
                      Plus5Min   -> ({m | moment=addMinute m.moment 5}, Cmd.none)
                      Plus15Min  -> ({m | moment=addMinute m.moment 15}, Cmd.none)
                      PlusHour   -> ({m | moment=addMinute m.moment 60}, Cmd.none)
    DayChange d    -> ({m | moment={day=d, hour = m.moment.hour, minute=m.moment.minute}}, Cmd.none)
    StopChange x d -> ((case d of
                        -- Halte changed, needs to be revalidated
                        Departure   -> {m | departure   = NoStop x}
                        Destination -> {m | destination = NoStop x}
                      ), Cmd.none)
    BusChange x    -> ({m | bus = NoBus x}, Cmd.none)
    _              -> (m, Cmd.none)

requestStops : Cmd Msg
-- Todo: http request the stops (and buses afterwards), for demo hardcode some examples
requestStops = Cmd.none

view : Model -> Html Msg
view m = div [] [button [onClick (TimeChange Minus5Min)] [text "-5"],
                      button [onClick (TimeChange Minus15Min)] [text "-15"],
                      button [onClick (TimeChange MinusHour)] [text "-60"],
                      text ((if m.moment.hour < 10 then "0" else "") ++ String.fromInt m.moment.hour ++ ":" ++ (if m.moment.minute < 10 then "0" else "") ++ String.fromInt m.moment.minute),
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
                              text (today m.moment.day)],
                      div [] [input [placeholder "Vertrekhalte",
                                     value (case m.departure of
                                              Stop i -> String.fromInt i
                                              NoStop x -> x),
                                     onInput (\x -> StopChange x Departure)] [],
                              button [onClick (ValidateStop Departure)] [text "Check"]],
                      div [] [button [onClick ReverseStops] [text "⇅"]],
                      div [] [input [placeholder "Aankomsthalte",
                                     value (case m.destination of
                                              Stop i -> String.fromInt i
                                              NoStop x -> x),
                                     onInput (\x -> StopChange x Destination)] [],
                              button [onClick (ValidateStop Destination)] [text "Check"]],
                      div [] [input [placeholder "Bus",
                                     value (case m.bus of
                                              Bus i -> String.fromInt i
                                              NoBus x -> x),
                                     onInput (\x -> BusChange x)] [],
                              button [onClick ValidateBus] [text "Check"]],
                      div [] [button [onClick CalcDelay] [text "Voorspel vertraging"]]]
subMinute : {day : Time.Weekday, hour : Int, minute : Int} -> Int -> {day : Time.Weekday, hour : Int, minute : Int}
subMinute m x = {m | hour=if m.minute - x < 0 then (if m.hour - 1 < 0 then m.hour + 23 else m.hour - 1) else m.hour, minute=if m.minute - x < 0 then m.minute - x + 60 else m.minute - x}

addMinute : {day : Time.Weekday, hour : Int, minute : Int} -> Int -> {day : Time.Weekday, hour : Int, minute : Int}
addMinute m x = {m | hour=if m.minute + x > 59 then (if m.hour + 1 > 23 then m.hour - 23 else m.hour + 1) else m.hour, minute=if m.minute + x > 59 then m.minute + x - 60 else m.minute + x}

today : Time.Weekday -> String
today t = (case t of
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