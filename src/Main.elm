module Main exposing (..)

import Platform
import Dict exposing (Dict)
import Date exposing (Date)
import Task
import Json.Decode
import Json.Encode as J
import Ports
import Types exposing (..)
import String as S
import List as L
import Dict as D
import Maybe as M


(.) =
  (<<)
infixr 9 .


($) =
  (<|)
infixr 0 $


echo : Date -> Conn -> Response
echo date { req } =
  respondTxt 200
    $ S.join " "
      [ toString date
      , toString req.method
      , req.pathname
      ]


greet : Conn -> Response
greet { req } =
  let
    name =
      M.withDefault "stranger" . D.get "name"

    msg =
      "Hello, " ++ (name req.queryParams) ++ "!"
  in
    respondJson 200
      $ json [ ( "message", J.string msg ) ]


notFound : Conn -> Response
notFound { req } =
  respondTxt 404
    $ S.join " "
      [ "Not found: "
      , toString req.method
      , req.pathname
      ]



-- Msg / update / main


type alias Conn =
  ConnState ()


type Msg
  = Echo Date
  | Greet
  | NotFound
  | Error String


route : Request -> Cmd Msg
route { method, pathname } =
  case ( method, pathname ) of
    ( GET, "/echo" ) ->
      Task.attempt (mapOk Echo) Date.now

    ( GET, "/greet" ) ->
      cmd Greet

    _ ->
      cmd NotFound


update : Msg -> Conn -> ( Conn, Cmd Msg )
update msg conn =
  case msg of
    Echo now ->
      conn ! [ respond $ echo now conn ]

    Greet ->
      conn ! [ respond $ greet conn ]

    NotFound ->
      conn ! [ respond $ notFound conn ]

    Error err ->
      conn ! [ fail err ]


main : Program RawConn Conn Msg
main =
  let
    init : RawConn -> ( Conn, Cmd Msg )
    init raw =
      case buildConn raw of
        Err err ->
          -- TODO
          emptyConn () ! [ fail err ]

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
          Result.map mkReq $ parseHttpMethod req.method

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


respond : Response -> Cmd msg
respond =
  Ports.respond


fail : String -> Cmd msg
fail err =
  Ports.respond . respondJson 500 $ json [ ( "error", J.string err ) ]


cmd : msg -> Cmd msg
cmd msg =
  msg
    |> Task.succeed
    |> Task.perform identity


json : List ( String, J.Value ) -> String
json =
  J.encode 0 . J.object


respondJson : Int -> String -> Response
respondJson =
  respondContentType "application/json"


respondTxt : Int -> String -> Response
respondTxt =
  respondContentType "text/plain"


respondContentType : String -> Int -> String -> Response
respondContentType ct status body =
  Response status [ ( "Content-Type", ct ) ] $ Just body


mapOk : (a -> Msg) -> Result x a -> Msg
mapOk msg r =
  case r of
    Ok a ->
      msg a

    Err err ->
      Error $ toString err


parseConfig : RawConfig -> Result String Config
parseConfig { env } =
  case S.toUpper env of
    "DEV" ->
      Ok $ Config Dev

    "TEST" ->
      Ok $ Config Test

    "PROD" ->
      Ok $ Config Prod

    _ ->
      Err $ "Invalid Env: `" ++ env ++ "`"


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
      Err $ "Invalid HTTP method: `" ++ str ++ "`"
