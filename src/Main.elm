module Main exposing (..)

import Platform
import Dict exposing (Dict)
import Date exposing (Date)
import Task
import Json.Decode
import Json.Encode as J
import Ports
import Types exposing (Output)
import String as S
import List as L
import Dict as D
import Maybe as M


(.) =
  (<<)


($) =
  (<|)


echo : Date -> Conn -> String
echo date { req } =
  S.join " "
    [ toString date
    , toString req.method
    , req.pathname
    ]


greet : Conn -> String
greet { req } =
  let
    name =
      M.withDefault "stranger" . D.get "name"
  in
    "Hello, " ++ (name req.queryParams) ++ "!"


notFound : Conn -> String
notFound { req } =
  S.join " "
    [ "Not found: "
    , toString req.method
    , req.pathname
    ]



-- Msg / update / main


type alias Conn =
  ConnState ()


type Msg
  = Echo (Result String Date)
  | Greet
  | NotFound


route : Request -> Cmd Msg
route { method, pathname } =
  case ( method, pathname ) of
    ( GET, "/echo" ) ->
      Task.attempt Echo Date.now

    ( GET, "/greet" ) ->
      cmd Greet

    _ ->
      cmd NotFound


update : Msg -> Conn -> ( Conn, Cmd Msg )
update msg conn =
  case msg of
    Echo (Ok now) ->
      conn ! [ return <| echo now conn ]

    Echo (Err err) ->
      conn ! [ fail err ]

    Greet ->
      conn ! [ return <| greet conn ]

    NotFound ->
      conn ! [ return <| notFound conn ]


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
          conn ! [ route conn.req ]

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
    Platform.programWithFlags
      { init = init
      , update = update
      , subscriptions = \_ -> Sub.none
      }



-- helpers


return : String -> Cmd msg
return =
  Ports.exit << success


fail : String -> Cmd msg
fail =
  Ports.exit << failure


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



-- type Response fmt
--   = NoContent
--   | Response (PopulatedResponse fmt)
--
--
-- type alias PopulatedResponse fmt =
--   { status : HttpStatus
--   , headers : Headers
--   , body : fmt
--   }
--
--
-- type
--   HttpStatus
--   -- TODO Int?
--   = Ok200
--   | Accepted201
