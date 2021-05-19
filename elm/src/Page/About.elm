module Page.About exposing (Model, Msg, init, subscriptions, update, view)

import Bool.Extra exposing (ifElse)
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table exposing (simpleTable, simpleThead, tbody, td, th, thead, tr)
import Bootstrap.Utilities.Spacing as Spacing
import Common exposing (commify)
import Config exposing (apiServer)
import Debug
import Html exposing (Html, a, b, br, div, h1, img, text)
import Html.Attributes exposing (class, for, href, src, style, target, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline
import RemoteData exposing (RemoteData, WebData)
import Route
import Session exposing (Session, SessionUser(..))
import Set
import Table
import Task
import Types exposing (SearchParams)
import Url.Builder


type alias Model =
    { session : Session
    , dataload : WebData Dataload
    }


type alias Dataload =
    { numStudies : Int
    , updatedOn : String
    }


type Msg
    = DataloadResponse (WebData Dataload)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , dataload = RemoteData.NotAsked
      }
    , Cmd.batch [ getDataload ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataloadResponse data ->
            ( { model | dataload = data }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        ( numStudies, updatedOn ) =
            case model.dataload of
                RemoteData.Success data ->
                    ( data.numStudies, data.updatedOn )

                _ ->
                    ( 0, "unknown" )
    in
    Grid.container []
        [ text "This is a search interface created by "
        , a [ href "http://c-path.org" ]
            [ text "The Critical Path Institute" ]
        , text " to the data that is available from "
        , a [ href "http://clinicaltrials.gov" ]
            [ text "ClinicalTrials.gov" ]
        , text ". "
        , text <|
            "This database contains "
                ++ commify numStudies
                ++ " studies and was last updated on "
                ++ updatedOn
                ++ "."
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getDataload : Cmd Msg
getDataload =
    Http.get
        { url = apiServer ++ "/dataload"
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> DataloadResponse)
                decoderDataload
        }


decoderDataload : Decoder Dataload
decoderDataload =
    Json.Decode.succeed Dataload
        |> Json.Decode.Pipeline.required "num_studies" int
        |> Json.Decode.Pipeline.required "updated_on" string
