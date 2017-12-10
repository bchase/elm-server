module Helpers exposing (defaultMain, mainForUpdate, return, fail)

import Platform exposing (programWithFlags)
import Task
import Json.Decode
import Ports exposing (exit)
import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)


-- default implentation for `main` & `update`


type DefaultMsg
  = Start


type alias Solver =
  Input -> String


defaultMain : Solver -> Program Flags Model DefaultMsg
defaultMain solve =
  mainForUpdate (defaultUpdate solve) Start


defaultUpdate : Solver -> InnerUpdateFunc DefaultMsg
defaultUpdate solve _ input =
  () ! [ return <| solve input ]



-- public helpers


mainForUpdate : InnerUpdateFunc msg -> msg -> Program Flags Model msg
mainForUpdate update startMsg =
  let
    init : Flags -> ( Model, Cmd msg )
    init flags =
      parseFlags flags ! [ cmd startMsg ]
  in
    programWithFlags
      { init = init
      , update = parseInputAnd update
      , subscriptions = \_ -> Sub.none
      }


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
