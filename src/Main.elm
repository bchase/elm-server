module Main exposing (..)

import Platform exposing (programWithFlags)
import Dict exposing (Dict)
import Date exposing (Date)
import Task
import Json.Decode
import Ports exposing (exit)
import Types exposing (Flags, Input, Model, Mode(..), AB(..), Output)
import String as S
import List as L
import Dict as D


echo : Date -> Conn -> String
echo date { req } =
  S.join " "
    [ toString date
    , toString req.method
    , req.pathname
    ]



-- Msg / update / main


type alias Conn =
  ConnState ()


type Msg
  = Start
  | Now (Result String Date)


update : Msg -> Conn -> ( Conn, Cmd Msg )
update msg conn =
  case msg of
    Start ->
      conn ! [ Task.attempt Now Date.now ]

    Now (Ok now) ->
      conn ! [ return <| echo now conn ]

    Now (Err err) ->
      conn ! [ fail err ]


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
        Result.map3 ConnState cfg_ req_ (Ok ())
  in
    programWithFlags
      { init = init
      , update = update
      , subscriptions = \_ -> Sub.none
      }



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

    "POST" ->
      Ok POST

    "PUT" ->
      Ok PUT

    "PATCH" ->
      Ok PATCH

    "DELETE" ->
      Ok DELETE

    "HEAD" ->
      Ok HEAD

    _ ->
      Err <| "Invalid HTTP method: `" ++ str ++ "`"



-- raw conn


type alias RawConn =
  { cfg : RawConfig
  , req : RawRequest
  }


type alias RawConfig =
  { env : String
  }


type alias RawRequest =
  { headers : List ( String, String )
  , method : String
  , pathname : String
  , queryParams : List ( String, String )
  , body : Maybe String
  }



-- conn


type alias ConnState state =
  { cfg : Config
  , req : Request
  , state : state
  }


type alias Config =
  { env : Env
  }


type Env
  = Dev
  | Test
  | Prod


type alias Request =
  { headers : Dict String String
  , method : HttpMethod
  , pathname : String
  , queryParams : Dict String String
  , body : Maybe String
  }


type HttpMethod
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | HEAD


emptyConn : Conn
emptyConn =
  ConnState emptyConfig emptyRequest ()


emptyConfig : Config
emptyConfig =
  { env = Prod
  }


emptyRequest : Request
emptyRequest =
  Request Dict.empty GET "/" Dict.empty Nothing
