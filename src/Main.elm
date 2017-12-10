module Main exposing (..)

import Platform exposing (programWithFlags)
import Dict exposing (Dict)
import Task
import Json.Decode
import Ports exposing (exit)
import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)
import String as S
import List as L
import Dict as D


-- solution


action : Conn -> String
action { req } =
  "received input: " ++ req.pathname



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
  , queryParams : Dict String String
  , body : Maybe String
  }


emptyRequest : Request
emptyRequest =
  Request Dict.empty GET "/" Dict.empty Nothing


type HttpMethod
  = GET


type alias RawConfig =
  { env : String
  }


type alias Config =
  { env : Env
  }


type Env
  = Dev
  | Test
  | Prod


type alias Conn =
  { cfg : Config
  , req : Request
  }


emptyConfig : Config
emptyConfig =
  { env = Prod
  }


emptyConn : Conn
emptyConn =
  Conn emptyConfig emptyRequest


update : Msg -> Conn -> ( Conn, Cmd Msg )
update msg conn =
  case msg of
    Start ->
      conn ! [ return <| action (Debug.log "conn" conn) ]


main : Program RawConn Conn Msg
main =
  let
    init : RawConn -> ( Conn, Cmd Msg )
    init raw =
      case buildConn raw of
        Err err ->
          -- TODO
          emptyConn ! [ fail err ]

        Ok conn ->
          conn ! [ cmd Start ]

    buildConn : RawConn -> Result String Conn
    buildConn { cfg, req } =
      let
        mkReq m =
          { headers = Dict.fromList req.headers
          , method = m
          , pathname = req.pathname
          , queryParams = Dict.fromList req.queryParams
          , body = req.body
          }

        req_ =
          Result.map mkReq <| parseHttpMethod req.method

        cfg_ =
          -- TODO fail sooner
          parseConfig cfg
      in
        Result.map2 Conn cfg_ req_
  in
    programWithFlags
      { init = init
      , update = update
      , subscriptions = \_ -> Sub.none
      }


parseConfig : RawConfig -> Result String Config
parseConfig { env } =
  case S.toUpper env of
    "DEV" ->
      Ok <| Config Dev

    "TEST" ->
      Ok <| Config Test

    "PROD" ->
      Ok <| Config Prod

    _ ->
      Err <| "Invalid Env: `" ++ env ++ "`"


parseHttpMethod : String -> Result String HttpMethod
parseHttpMethod str =
  case S.toUpper str of
    "GET" ->
      Ok GET

    _ ->
      Err <| "Invalid HTTP method: `" ++ str ++ "`"



-- helpers


return : String -> Cmd msg
return =
  exit << success


fail : String -> Cmd msg
fail =
  exit << failure


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
