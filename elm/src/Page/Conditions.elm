module Page.Conditions exposing (Model, Msg, init, subscriptions, update, view)

import Bool.Extra exposing (ifElse)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
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
import Maybe.Extra exposing (isNothing)
import RemoteData exposing (RemoteData, WebData)
import Route
import Session exposing (Session)
import Table
import Url.Builder


type alias Model =
    { session : Session
    , conditionsFilter : Maybe String
    , filterBool : Bool
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
    | SetFilterBool Bool
    | SetTableState Table.State


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , conditionsFilter = Nothing
      , filterBool = False
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
            ( model, doSearch model )

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

        SetFilterBool toggle ->
            ( { model | filterBool = toggle }, Cmd.none )

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

        form =
            Form.formInline []
                [ Input.text
                    [ Input.attrs
                        [ placeholder "Search", onInput SetConditionsFilter ]
                    ]
                , Checkbox.checkbox
                    [ Checkbox.id "chkBool"
                    , Checkbox.checked model.filterBool
                    , Checkbox.onCheck SetFilterBool
                    , Checkbox.attrs [ Spacing.mx1 ]
                    ]
                    "Boolean"
                , Button.button
                    [ Button.primary
                    , Button.onClick Search
                    , Button.attrs [ Spacing.mx1 ]
                    , Button.disabled (isNothing model.conditionsFilter)
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
    let
        linkCol : Condition -> Table.HtmlDetails Msg
        linkCol condition =
            Table.HtmlDetails []
                [ a
                    [ href
                        (Route.routeToString
                            (Route.Home (Just condition.conditionName))
                        )
                    ]
                    [ text condition.conditionName ]
                ]
    in
    Table.config
        { toId = .conditionName
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Condition" .conditionName
            , Table.intColumn "Num Studies" .numStudies
            ]
        }



--[ Table.veryCustomColumn
--    { name = "Condition Name"
--    , viewData = linkCol
--    , sorter = Table.increasingOrDecreasingBy .conditionName
--    }


doSearch : Model -> Cmd Msg
doSearch model =
    let
        url =
            apiServer
                ++ "/conditions?name="
                ++ Maybe.withDefault "" model.conditionsFilter
                ++ ifElse "&bool_search=1" "" model.filterBool
    in
    Http.get
        { url = url
        , expect =
            -- Http.expectJson
            expectJson
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


expectJson : (Result Http.Error a -> msg) -> Json.Decode.Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    -- Err (Http.BadStatus metadata.statusCode)
                    Err (Http.BadBody body)

                Http.GoodStatus_ metadata body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (Json.Decode.errorToString err))
