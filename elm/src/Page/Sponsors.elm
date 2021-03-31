module Page.Sponsors exposing (Model, Msg, init, subscriptions, update, view)

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
    , sponsorsFilter : Maybe String
    , filterBool : Bool
    , sponsors : WebData (List Sponsor)
    , tableState : Table.State
    }


type alias Sponsor =
    { sponsorId : Int
    , sponsorName : String
    , numStudies : Int
    }


type Msg
    = SponsorsResponse (WebData (List Sponsor))
    | Search
    | SetSponsorsFilter String
    | SetFilterBool Bool
    | SetTableState Table.State


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , sponsorsFilter = Nothing
      , filterBool = False
      , sponsors = RemoteData.NotAsked
      , tableState = Table.initialSort "sponsorName"
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SponsorsResponse data ->
            ( { model | sponsors = data }
            , Cmd.none
            )

        Search ->
            ( model, doSearch model )

        SetSponsorsFilter text ->
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
            ( { model | sponsorsFilter = newFilter }, Cmd.none )

        SetFilterBool toggle ->
            ( { model | filterBool = toggle }, Cmd.none )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        viewSponsors =
            case model.sponsors of
                RemoteData.NotAsked ->
                    div [] [ text "" ]

                RemoteData.Loading ->
                    div [] [ text "Fetching sponsors..." ]

                RemoteData.Failure httpError ->
                    div [] [ text (viewHttpErrorMessage httpError) ]

                RemoteData.Success sponsors ->
                    let
                        numSponsors =
                            List.length sponsors
                    in
                    div []
                        [ h1 []
                            [ text <|
                                "Sponsors ("
                                    ++ commify numSponsors
                                    ++ ")"
                            ]
                        , div []
                            [ Table.view tableConfig model.tableState sponsors
                            ]
                        ]

        form =
            Form.formInline []
                [ Input.text
                    [ Input.attrs
                        [ placeholder "Search", onInput SetSponsorsFilter ]
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
                    , Button.disabled (isNothing model.sponsorsFilter)
                    ]
                    [ text "Search" ]
                ]
    in
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ form
                , viewSponsors

                -- , text <| Maybe.withDefault "" model.sponsorsFilter
                ]
            ]
        ]


tableConfig : Table.Config Sponsor Msg
tableConfig =
    let
        linkCol : Sponsor -> Table.HtmlDetails Msg
        linkCol sponsor =
            Table.HtmlDetails []
                [ a
                    [ href
                        (Route.routeToString
                            (Route.Home (Just sponsor.sponsorName))
                        )
                    ]
                    [ text sponsor.sponsorName ]
                ]
    in
    Table.config
        { toId = .sponsorName
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Sponsor" .sponsorName
            , Table.intColumn "Num Studies" .numStudies
            ]
        }


doSearch : Model -> Cmd Msg
doSearch model =
    let
        sponsors =
            Url.Builder.string "name" <|
                Maybe.withDefault "" model.sponsorsFilter

        boolSearch =
            Url.Builder.int "bool_search" <|
                ifElse 1 0 model.filterBool

        params =
            Url.Builder.toQuery [ sponsors, boolSearch ]

        url =
            apiServer ++ "/sponsors" ++ params
    in
    Http.get
        { url = url
        , expect =
            -- Http.expectJson
            expectJson
                (RemoteData.fromResult >> SponsorsResponse)
                (Json.Decode.list decoderSponsor)
        }


decoderSponsor : Decoder Sponsor
decoderSponsor =
    Json.Decode.succeed Sponsor
        |> Json.Decode.Pipeline.required "sponsor_id" int
        |> Json.Decode.Pipeline.required "sponsor_name" string
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
