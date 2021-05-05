module Page.About exposing (Model, Msg, init, subscriptions, update, view)

import Bool.Extra exposing (ifElse)
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table exposing (simpleTable, simpleThead, tbody, td, th, thead, tr)
import Bootstrap.Utilities.Spacing as Spacing
import Debug
import Html exposing (Html, a, b, br, div, h1, img, text)
import Html.Attributes exposing (class, for, href, src, style, target, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
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
    }


type Msg
    = NoOp


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Grid.container []
        [ text "This is a search interface created by "
        , a [ href "http://c-path.org" ]
            [ text "The Critical Path Institute" ]
        , text " to the data that is available from "
        , a [ href "http://clinicaltrials.gov" ]
            [ text "ClinicalTrials.gov" ]
        , text "."
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
