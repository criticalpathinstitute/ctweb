port module Credentials exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra exposing (maybe)
import User as User exposing (User)



-- IMPORTANT!!!
-- When changing the structure of this record be sure to change
-- the cookie name to prevent errors when decoding the old cookies
-- (can manifest as infinite login loop)


type alias Credentials =
    { token : String
    , refreshToken : String
    , expiresIn : Maybe Int
    , user : Maybe User
    }


default : Credentials
default =
    { token = ""
    , refreshToken = ""
    , expiresIn = Nothing
    , user = Nothing
    }


decoder : Decoder Credentials
decoder =
    Decode.succeed Credentials
        |> optional "token" Decode.string ""
        |> optional "refreshToken" Decode.string ""
        |> optional "expiresIn" (Decode.nullable Decode.int) Nothing
        |> optional "user" (Decode.nullable User.userDecoder) Nothing


encode : Credentials -> Value
encode cred =
    Encode.object
        [ ( "token", Encode.string cred.token )
        , ( "refreshToken", Encode.string cred.refreshToken )
        , ( "expiresIn", maybe Encode.int cred.expiresIn )
        , ( "user", maybe User.encodeUser cred.user )
        ]


store : Credentials -> Cmd msg
store cred =
    encode cred
        |> Just
        |> storeCredentials


port storeCredentials : Maybe Value -> Cmd msg


port onCredentialsChange : (String -> msg) -> Sub msg
