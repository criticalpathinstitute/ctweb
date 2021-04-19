module Types exposing (..)


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
