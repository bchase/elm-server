module Main exposing (..)

import Platform exposing (programWithFlags)
import Task
import Json.Decode
import Ports exposing (exit)
import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)


-- solution


action : Conn -> String
action _ =
  "received input: " ++ "NOT YET"



-- Msg / update / main


type Msg
  = Start


type alias RawConn =
  { input : String
  }


type alias Conn =
  { input : String
  }


update : Msg -> Conn -> ( Conn, Cmd Msg )
update msg conn =
  case msg of
    Start ->
      conn ! [ return <| action conn ]


main : Program RawConn Conn Msg
main =
  let
    init : RawConn -> ( Conn, Cmd Msg )
    init raw =
      let
        conn =
          Conn raw.input
      in
        conn ! [ cmd Start ]
  in
    programWithFlags
      { init =
        init
        -- , update = parseInputAnd update
      , update = update
      , subscriptions = \_ -> Sub.none
      }



-- default implentation for `main` & `update`


return : String -> Cmd msg
return =
  exit << success


fail : String -> Cmd msg
fail =
  exit << failure



-- private helpers


type alias OuterUpdateFunc msg =
  msg -> Model -> ( Model, Cmd msg )


type alias InnerUpdateFunc msg =
  msg -> Input -> ( (), Cmd msg )


success : String -> Output
success =
  Output True


failure : String -> Output
failure =
  Output False


cmd : msg -> Cmd msg
cmd msg =
  msg
    |> Task.succeed
    |> Task.perform identity


parseInputAnd : InnerUpdateFunc msg -> OuterUpdateFunc msg
parseInputAnd update msg model =
  case ( msg, model ) of
    ( _, Err flags ) ->
      model ! [ fail <| "Unable to parse flags" ++ toString flags ]

    ( msg, Ok input ) ->
      let
        cmd =
          Tuple.second <| update msg input
      in
        model ! [ cmd ]


parseFlags : Flags -> Model
parseFlags ({ mode, ab, input } as flags) =
  let
    ab_ =
      case String.toUpper ab of
        "A" ->
          Just A

        "B" ->
          Just B

        _ ->
          Nothing

    mode_ =
      case String.toUpper mode of
        "RUN" ->
          Just Run

        "TEST" ->
          Just Test

        _ ->
          Nothing

    model =
      Maybe.map3 (Input) mode_ ab_ (Just input)
  in
    Result.fromMaybe flags model
