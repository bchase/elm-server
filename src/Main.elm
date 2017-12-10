module Main exposing (..)

import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)
import Helpers as H


-- solution


solve : Input -> String
solve { mode, ab, input } =
  case ( mode, ab ) of
    ( _, _ ) ->
      (\str -> "received input: " ++ str) input



-- Msg / update / main


type Msg
  = Start


update : Msg -> Input -> ( (), Cmd Msg )
update msg input =
  case msg of
    Start ->
      () ! [ H.return <| solve input ]


main =
  H.mainForUpdate update Start
