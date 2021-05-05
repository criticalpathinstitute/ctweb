module User exposing (..)

import Config exposing (apiServer)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)



-- TYPES --
-- IMPORTANT!!!
-- This type exists in the Session.
-- When changing the structure of this record be sure to change the cookie name
-- to prevent errors when decoding the old cookies (can manifest as infinite login loop)


type alias User =
    { name : String
    , nickname : String
    , picture : String
    }



-- SERIALIZATION --


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "name" Decode.string
        |> required "nickname" Decode.string
        |> required "picture" Decode.string


encodeUser : User -> Value
encodeUser user =
    Encode.object
        [ ( "name", Encode.string user.name )
        , ( "nickname", Encode.string user.nickname )
        , ( "picture", Encode.string user.picture )
        ]
