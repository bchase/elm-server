module Types exposing (..)

import Dict exposing (Dict)


type alias Output =
  -- TODO rm after Response
  { success : Bool
  , payload : String
  }



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
  -- TODO
  -- { host :
  -- , port :
  -- , protocol :
  -- , remoteAddress :
  -- , rawPath :
  -- , rawQueryString :
  -- }
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


emptyConn : s -> ConnState s
emptyConn s =
  ConnState emptyConfig emptyRequest s


emptyConfig : Config
emptyConfig =
  { env = Prod
  }


emptyRequest : Request
emptyRequest =
  Request Dict.empty GET "/" Dict.empty Nothing
