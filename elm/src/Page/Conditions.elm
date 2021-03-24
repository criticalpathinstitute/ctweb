module Page.Conditions exposing (Model, Msg, init, subscriptions, update, view)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Common exposing (commify, viewHttpErrorMessage)
import Config exposing (apiServer, serverAddress)
import Html exposing (Html, a, div, h1, h2, li, text, ul)
import Html.Attributes exposing (for, href, placeholder, style, target)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (RemoteData, WebData)
import Route
import Session exposing (Session)
import Table
import Url.Builder


type alias Model =
    { session : Session
    , conditionsFilter : Maybe String
    , conditions : WebData (List Condition)
    , tableState : Table.State
    }


type alias Condition =
    { conditionId : Int
    , conditionName : String
    , numStudies : Int
    }


type Msg
    = ConditionsResponse (WebData (List Condition))
    | Search
    | SetConditionsFilter String
    | SetTableState Table.State


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , conditionsFilter = Nothing
      , conditions = RemoteData.NotAsked
      , tableState = Table.initialSort "conditionName"
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConditionsResponse data ->
            ( { model | conditions = data }
            , Cmd.none
            )

        Search ->
            ( model, doSearch model.conditionsFilter )

        SetConditionsFilter text ->
            let
                trimmed =
                    String.trim text

                newFilter =
                    case String.length trimmed of
                        0 ->
                            Nothing

                        _ ->
                            Just trimmed
            in
            ( { model | conditionsFilter = newFilter }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        viewConditions =
            case model.conditions of
                RemoteData.NotAsked ->
                    div [] [ text "" ]

                RemoteData.Loading ->
                    div [] [ text "Fetching conditions..." ]

                RemoteData.Failure httpError ->
                    div [] [ text (viewHttpErrorMessage httpError) ]

                RemoteData.Success conditions ->
                    let
                        numConditions =
                            List.length conditions
                    in
                    div []
                        [ h1 []
                            [ text <|
                                "Conditions ("
                                    ++ String.fromInt numConditions
                                    ++ ")"
                            ]
                        , div []
                            [ Table.view tableConfig model.tableState conditions
                            ]
                        ]

        hasFilter =
            case model.conditionsFilter of
                Just _ ->
                    True

                _ ->
                    False

        form =
            Form.formInline []
                [ Input.text
                    [ Input.attrs
                        [ placeholder "Search", onInput SetConditionsFilter ]
                    ]
                , Button.button
                    [ Button.primary
                    , Button.onClick Search
                    , Button.attrs [ Spacing.mx1 ]
                    , Button.disabled (not hasFilter)
                    ]
                    [ text "Search" ]
                ]
    in
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ form
                , viewConditions
                ]
            ]
        ]


tableConfig : Table.Config Condition Msg
tableConfig =
    Table.config
        { toId = .conditionName
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Condition Name" .conditionName
            , Table.intColumn "Num Studies" .numStudies
            ]
        }


doSearch : Maybe String -> Cmd Msg
doSearch conditionsFilter =
    let
        conditions =
            case conditionsFilter of
                Just c ->
                    c

                _ ->
                    ""

        url =
            apiServer ++ "/conditions?name=" ++ conditions
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> ConditionsResponse)
                (Json.Decode.list decoderCondition)
        }


decoderCondition : Decoder Condition
decoderCondition =
    Json.Decode.succeed Condition
        |> Json.Decode.Pipeline.required "condition_id" int
        |> Json.Decode.Pipeline.required "condition_name" string
        |> Json.Decode.Pipeline.required "num_studies" int


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
