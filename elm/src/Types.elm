module Types exposing (..)

import OAuth
import OAuth.Implicit as OAuth
import User exposing (User)


type alias SearchParams =
    { searchName : Maybe String
    , fullText : Maybe String
    , fullTextBool : Bool
    , conditions : Maybe String
    , conditionsBool : Bool
    , sponsors : Maybe String
    , sponsorsBool : Bool
    , phaseIds : List Int
    , studyTypeIds : List Int
    , enrollment : Int
    }


type Flow
    = Idle
    | Authorized OAuth.Token
    | Done User
    | Errored FlowError


type FlowError
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo
