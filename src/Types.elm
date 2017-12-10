module Types exposing (..)

import Dict exposing (Dict)


type alias Conn =
  -- TODO replace Flags
  { cfg : Config
  , req : Request
  , resp : Response
  }


type alias Request =
  -- { host :
  -- , port :
  -- , protocol :
  -- , remoteAddress :
  -- , rawPath :
  -- , rawQueryString :
  -- }
  { method : HttpMethod
  , path : Path
  , params : Params
  }


type alias Path =
  -- TODO List?
  String


type alias Params =
  Dict String String


type alias Headers =
  Dict String String


type Response fmt
  = NoContent
  | Response (PopulatedResponse fmt)


type alias PopulatedResponse fmt =
  { status : HttpStatus
  , headers : Headers
  , body : fmt
  }


type
  HttpStatus
  -- TODO Int?
  = Ok200
  | Accepted201


type HttpMethod
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | HEAD


type alias Config =
  { env : Env
  }


type Env
  = Dev
  | Test_
    -- TODO Test
  | Prod



-- TODO DELETE/ADJUST OLD BELOW


type alias Flags =
  { mode : String
  , ab : String
  , input : String
  }


type alias Input =
  { mode : Mode
  , ab : AB
  , input : String
  }


type alias Model =
  Result Flags Input


type Mode
  = Run
  | Test


type AB
  = A
  | B


type alias Output =
  { success : Bool
  , payload : String
  }
