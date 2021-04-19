module Page.Searches exposing (InternalMsg, Model, Msg, Translator, init, subscriptions, translator, update, view)

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
import Types exposing (SearchParams)
import Url.Builder


type alias Model =
    { session : Session
    , searches : WebData (List SavedSearch)
    , tableState : Table.State
    }


type alias SavedSearch =
    { savedSearchId : Int
    , searchName : String
    , fullText : String
    , fullTextBool : Int
    , sponsors : String
    , sponsorsBool : Int
    , conditions : String
    , conditionsBool : Int
    , phaseIds : String
    , studyTypeIds : String
    , enrollment : Int
    }


type OutMsg
    = SetSearchParams SearchParams


type InternalMsg
    = SavedSearchesResponse (WebData (List SavedSearch))
    | SetTableState Table.State


type Msg
    = ForSelf InternalMsg
    | ForParent OutMsg


type alias TranslationDictionary msg =
    { onInternalMessage : InternalMsg -> msg
    , onSetSearchParams : SearchParams -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : TranslationDictionary msg -> Translator msg
translator { onInternalMessage, onSetSearchParams } msg =
    case msg of
        ForSelf internal ->
            onInternalMessage internal

        ForParent (SetSearchParams val) ->
            onSetSearchParams val


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , searches = RemoteData.NotAsked
      , tableState = Table.initialSort "searchName"
      }
    , getSearches
    )


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SavedSearchesResponse data ->
            ( { model | searches = data }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        viewSearches =
            case model.searches of
                RemoteData.NotAsked ->
                    div [] [ text "" ]

                RemoteData.Loading ->
                    div [] [ text "Fetching searches..." ]

                RemoteData.Failure httpError ->
                    div [] [ text (viewHttpErrorMessage httpError) ]

                RemoteData.Success searches ->
                    let
                        numSearches =
                            List.length searches
                    in
                    div []
                        [ h1 []
                            [ text <|
                                "Searches ("
                                    ++ commify numSearches
                                    ++ ")"
                            ]
                        , div []
                            [ Table.view tableConfig model.tableState searches
                            ]
                        ]
    in
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ viewSearches
                ]
            ]
        ]



-- tableConfig : Table.Config SavedSearch InternalMsg


tableConfig =
    let
        strToMaybe s =
            if String.length s > 0 then
                Just s

            else
                Nothing

        intToBool i =
            i == 1

        strToIntList s =
            String.split "," s
                |> List.map (\v -> Maybe.withDefault 0 (String.toInt v))
                |> List.filter (\v -> v > 0)

        params item =
            { searchName = strToMaybe item.searchName
            , fullText = strToMaybe item.fullText
            , fullTextBool = intToBool item.fullTextBool
            , conditions = strToMaybe item.conditions
            , conditionsBool = intToBool item.conditionsBool
            , sponsors = strToMaybe item.sponsors
            , sponsorsBool = intToBool item.sponsorsBool
            , phaseIds = strToIntList item.phaseIds
            , studyTypeIds = strToIntList item.studyTypeIds
            , enrollment = item.enrollment
            }

        button item =
            Button.button
                [ Button.primary
                , Button.onClick (ForParent (SetSearchParams (params item)))
                ]
                [ text "Search" ]

        linkCol : SavedSearch -> Table.HtmlDetails Msg
        linkCol item =
            Table.HtmlDetails [] [ button item ]
    in
    Table.config
        { toId = .searchName
        , toMsg = \x -> ForSelf (SetTableState x)
        , columns =
            [ Table.stringColumn "Search Name" .searchName
            , Table.stringColumn "Full Text" .fullText
            , Table.veryCustomColumn
                { name = "Link"
                , viewData = linkCol
                , sorter = Table.unsortable
                }
            ]
        }


decoderSavedSearch : Decoder SavedSearch
decoderSavedSearch =
    Json.Decode.succeed SavedSearch
        |> Json.Decode.Pipeline.required "saved_search_id" int
        |> Json.Decode.Pipeline.required "search_name" string
        |> Json.Decode.Pipeline.required "full_text" string
        |> Json.Decode.Pipeline.required "full_text_bool" int
        |> Json.Decode.Pipeline.required "sponsors" string
        |> Json.Decode.Pipeline.required "sponsors_bool" int
        |> Json.Decode.Pipeline.required "conditions" string
        |> Json.Decode.Pipeline.required "conditions_bool" int
        |> Json.Decode.Pipeline.required "phase_ids" string
        |> Json.Decode.Pipeline.required "study_type_ids" string
        |> Json.Decode.Pipeline.required "enrollment" int


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getSearches : Cmd Msg
getSearches =
    let
        url =
            apiServer ++ "/saved_searches"
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson
                (RemoteData.fromResult
                    >> (\x -> ForSelf (SavedSearchesResponse x))
                )
                (Json.Decode.list decoderSavedSearch)
        }
