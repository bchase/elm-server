module Main exposing (..)

import Platform exposing (programWithFlags)
import Dict exposing (Dict)
import Task
import Json.Decode
import Ports exposing (exit)
import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)


-- solution


action : Conn -> String
action { input } =
  "received input: " ++ input



-- Msg / update / main


type Msg
  = Start


type alias RawConn =
  { cfg : RawConfig
  , req : RawRequest
  }


type alias RawRequest =
  { headers : List ( String, String )
  , method : String
  , pathname : String
  , queryParams : List ( String, String )
  , body : Maybe String
  }


type alias Request =
  { headers : Dict String String
  , method : HttpMethod
  , pathname : String
  , qsp : Maybe (Dict String String)
  , body : Maybe String
  }


type HttpMethod
  = GET


type alias RawConfig =
  { env : String
  }


type alias Conn =
  { input : String
  }


emptyConn : Conn
emptyConn =
  Conn ""


update : Msg -> Conn -> ( Conn, Cmd Msg )
update msg conn =
  case msg of
    Start ->
      conn ! [ return <| action conn ]


main : Program RawConn Conn Msg
main =
  let
    buildConn : RawConn -> Conn
    buildConn { cfg, req } =
      -- let
      --   { headers, method, pathname, queryParams, body } =
      --     req
      -- in
      --   { headers = Dict.fromList headers
      --   , method
      --   , pathname = pathname
      --   , qsp =
      --   , body
      --   }
      { input = req.pathname
      }
  in
    programWithFlags
      { init =
        \raw -> buildConn (Debug.log "raw" raw) ! [ cmd Start ]
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
