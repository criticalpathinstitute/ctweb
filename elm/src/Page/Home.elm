module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table exposing (simpleThead, table, tbody, td, th, thead, tr)
import Bootstrap.Utilities.Spacing as Spacing
import Cart
import Common exposing (commify, viewHttpErrorMessage)
import Config exposing (apiServer, maxCartSize)
import Debug
import Html exposing (Html, a, b, br, div, h1, img, text)
import Html.Attributes exposing (class, for, href, src, style, target, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Regex
import RemoteData exposing (RemoteData, WebData)
import Route
import Session exposing (Session)
import Set
import Task
import Url.Builder


type alias Model =
    { errorMessage : Maybe String
    , phases : WebData (List Phase)
    , queryConditions : Maybe String
    , queryEnrollment : Maybe Int
    , querySelectedPhases : List Phase
    , querySponsors : Maybe String
    , queryText : Maybe String
    , searchResults : WebData (List Study)
    , selectedStudies : List Study
    , session : Session
    , summary : WebData Summary
    }


type alias Condition =
    { conditionId : Int
    , conditionName : String
    , numStudies : Int
    }


type alias Phase =
    { phaseId : Int
    , phaseName : String
    , numStudies : Int
    }


type alias Sponsor =
    { sponsorId : Int
    , sponsorName : String
    , numStudies : Int
    }


type alias Summary =
    { numStudies : Int
    }


type alias Study =
    { studyId : Int
    , nctId : String
    , title : String
    , detailedDescription : String
    }


type Msg
    = AddPhase String
    | CartMsg Cart.Msg
    | DoSearch
    | RemovePhase Phase
    | Reset
    | PhasesResponse (WebData (List Phase))
    | SummaryResponse (WebData Summary)
    | SetConditions String
    | SetSponsors String
    | SetQueryText String
    | SetEnrollment String
    | SearchResponse (WebData (List Study))


initialModel : Session -> Model
initialModel session =
    { errorMessage = Nothing
    , phases = RemoteData.NotAsked
    , queryConditions = Nothing
    , queryEnrollment = Nothing
    , querySelectedPhases = []
    , querySponsors = Nothing
    , queryText = Nothing
    , searchResults = RemoteData.NotAsked
    , selectedStudies = []
    , session = session
    , summary = RemoteData.NotAsked
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( initialModel session
    , Cmd.batch [ getSummary, getPhases ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPhase phaseId ->
            let
                currentPhaseIds =
                    Set.fromList <|
                        List.map (\p -> p.phaseId) model.querySelectedPhases

                currentPhases =
                    case model.phases of
                        RemoteData.Success data ->
                            data

                        _ ->
                            []

                newPhases =
                    case String.toInt phaseId of
                        Just newId ->
                            let
                                newPhaseIds =
                                    Set.insert newId currentPhaseIds
                            in
                            List.filter (\p -> Set.member p.phaseId newPhaseIds)
                                currentPhases

                        _ ->
                            currentPhases

                newModel =
                    { model | querySelectedPhases = newPhases }
            in
            ( newModel, Cmd.none )

        CartMsg subMsg ->
            let
                cart =
                    Session.getCart model.session

                updateCart =
                    case subMsg of
                        Cart.AddToCart idList ->
                            Cart.size cart + List.length idList <= maxCartSize

                        _ ->
                            True
            in
            if updateCart then
                let
                    newCart =
                        Cart.update subMsg cart

                    newSession =
                        Session.setCart model.session newCart
                in
                ( { model | session = newSession }
                , Cart.store newCart
                )

            else
                let
                    err =
                        "Too many files to add to the cart (>"
                            ++ String.fromInt maxCartSize
                            ++ "). Try constraining the search parameters."
                in
                ( { model | errorMessage = Just err }, Cmd.none )

        DoSearch ->
            ( { model | searchResults = RemoteData.Loading }, doSearch model )

        PhasesResponse data ->
            ( { model | phases = data }
            , Cmd.none
            )

        RemovePhase newPhase ->
            let
                newPhases =
                    List.filter
                        (\phase -> phase /= newPhase)
                        model.querySelectedPhases

                newModel =
                    { model | querySelectedPhases = newPhases }
            in
            ( newModel, Cmd.none )

        Reset ->
            ( initialModel model.session, Cmd.none )

        SummaryResponse data ->
            ( { model | summary = data }
            , Cmd.none
            )

        SearchResponse data ->
            ( { model | searchResults = data }
            , Cmd.none
            )

        SetConditions text ->
            ( { model | queryConditions = strToMaybe (String.toLower text) }
            , Cmd.none
            )

        SetSponsors text ->
            ( { model | querySponsors = strToMaybe (String.toLower text) }
            , Cmd.none
            )

        SetQueryText query ->
            ( { model | queryText = strToMaybe query }, Cmd.none )

        SetEnrollment enrollment ->
            ( { model | queryEnrollment = String.toInt enrollment }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        summary =
            case model.summary of
                RemoteData.NotAsked ->
                    "Not asked"

                RemoteData.Loading ->
                    "Loading data..."

                RemoteData.Failure httpError ->
                    viewHttpErrorMessage httpError

                RemoteData.Success data ->
                    "Search " ++ commify data.numStudies ++ " studies"

        empty =
            [ Select.item [ value "" ] [ text "--Select--" ] ]

        addAll =
            [ Select.item [ value "-1" ] [ text "[All]" ] ]

        mkPhasesSelect =
            let
                mkSelect data =
                    case List.length data of
                        0 ->
                            text "No phases"

                        _ ->
                            Select.select
                                [ Select.id "phase"
                                , Select.onChange AddPhase
                                ]
                                (empty ++ List.map mkSelectItem data)

                mkSelectItem phase =
                    Select.item
                        [ value <| String.fromInt phase.phaseId ]
                        [ text <|
                            phase.phaseName
                                ++ " ("
                                ++ String.fromInt phase.numStudies
                                ++ ")"
                        ]
            in
            case model.phases of
                RemoteData.Success data ->
                    mkSelect data

                RemoteData.Failure httpError ->
                    text (viewHttpErrorMessage httpError)

                _ ->
                    text "Loading phases..."

        viewPhase phase =
            Button.button
                [ Button.outlinePrimary
                , Button.onClick (RemovePhase phase)
                ]
                [ text (phase.phaseName ++ " ⦻") ]

        viewSelectedPhases =
            List.map viewPhase model.querySelectedPhases

        searchForm =
            Form.form [ onSubmit DoSearch ]
                [ Form.group []
                    [ Form.label [ for "text" ] [ text "Text:" ]
                    , Input.text
                        [ Input.attrs [ onInput SetQueryText ] ]
                    ]
                , Form.group []
                    ([ Form.label [ for "phases" ]
                        [ text "Phase:" ]
                     , mkPhasesSelect
                     ]
                        ++ viewSelectedPhases
                    )
                , Form.group []
                    [ Form.label [ for "enrollment" ]
                        [ text "Enrollment:" ]
                    , Input.text
                        [ Input.attrs [ onInput SetEnrollment ] ]
                    ]
                , Form.group []
                    [ Form.label [ for "condition" ]
                        [ text "Condition:" ]
                    , Input.text
                        [ Input.attrs [ onInput SetConditions ] ]
                    ]
                , Form.group []
                    [ Form.label [ for "sponsor" ]
                        [ text "Sponsor:" ]
                    , Input.text
                        [ Input.attrs [ onInput SetSponsors ] ]
                    ]
                , Button.button
                    [ Button.primary
                    , Button.onClick DoSearch
                    , Button.attrs [ Spacing.mx1 ]
                    ]
                    [ text "Submit" ]
                , Button.button
                    [ Button.secondary
                    , Button.onClick Reset
                    , Button.attrs [ Spacing.mx1 ]
                    ]
                    [ text "Clear" ]
                ]

        results =
            case model.searchResults of
                RemoteData.NotAsked ->
                    div [] [ text "" ]

                RemoteData.Loading ->
                    div [] [ img [ src "/assets/images/loading.gif" ] [] ]

                RemoteData.Failure httpError ->
                    div [] [ text (viewHttpErrorMessage httpError) ]

                RemoteData.Success studies ->
                    let
                        cart =
                            Session.getCart model.session

                        numStudies =
                            List.length studies

                        title =
                            "Search Results (" ++ commify numStudies ++ ")"

                        mkRow study =
                            tr []
                                [ td []
                                    [ Cart.addToCartButton cart study.studyId
                                        |> Html.map CartMsg
                                    ]
                                , td [] [ text study.title ]
                                , td []
                                    [ a
                                        [ Route.href
                                            (Route.Study study.nctId)
                                        , target "_blank"
                                        ]
                                        [ text study.nctId ]
                                    ]
                                ]

                        errorMessage =
                            div []
                                [ text <|
                                    Maybe.withDefault "" model.errorMessage
                                ]

                        resultsTable =
                            table
                                { options =
                                    [ Bootstrap.Table.striped ]
                                , thead =
                                    simpleThead
                                        [ th [] []
                                        , th [] [ text "Title" ]
                                        , th [] [ text "NCT ID" ]
                                        ]
                                , tbody =
                                    tbody []
                                        (List.map
                                            mkRow
                                            studies
                                        )
                                }

                        resultsDiv =
                            let
                                idList =
                                    List.map (\s -> s.studyId) studies

                                cartButton =
                                    div []
                                        [ Cart.addAllToCartButton cart idList
                                            |> Html.map CartMsg
                                        ]

                                body =
                                    case numStudies of
                                        0 ->
                                            div [] []

                                        _ ->
                                            div [] [ resultsTable ]
                            in
                            [ h1 [] [ text title ]
                            , errorMessage
                            , cartButton
                            , body
                            ]
                    in
                    div [] resultsDiv

        numSelectedStudies =
            List.length model.selectedStudies
    in
    Grid.container []
        [ Grid.row [ Row.centerMd ]
            [ Grid.col
                [ Col.xs, Col.md8 ]
                [ text summary ]
            ]
        , Grid.row []
            [ Grid.col [ Col.mdAuto ] [ searchForm ]
            ]
        , Grid.row []
            [ Grid.col [] [ results ]
            ]
        ]


filteredConditions : List Condition -> Maybe String -> List Condition
filteredConditions conditions conditionFilter =
    let
        regex =
            case conditionFilter of
                Just filter ->
                    Just
                        (Maybe.withDefault Regex.never
                            (Regex.fromString filter)
                        )

                _ ->
                    Nothing
    in
    case regex of
        Just re ->
            List.filter
                (\c ->
                    Regex.contains re
                        (String.toLower c.conditionName)
                )
                conditions

        _ ->
            []


getSummary : Cmd Msg
getSummary =
    Http.get
        { url = apiServer ++ "/summary"
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> SummaryResponse)
                decoderSummary
        }


getPhases : Cmd Msg
getPhases =
    Http.get
        { url = apiServer ++ "/phases"
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> PhasesResponse)
                (Json.Decode.list decoderPhase)
        }


doSearch : Model -> Cmd Msg
doSearch model =
    let
        builder ( label, value ) =
            case value of
                Just v ->
                    Just (Url.Builder.string label v)

                _ ->
                    Nothing

        enrollment =
            case model.queryEnrollment of
                Just n ->
                    Just (String.fromInt n)

                _ ->
                    Nothing

        phases =
            case List.length model.querySelectedPhases of
                0 ->
                    Nothing

                _ ->
                    Just
                        (String.join ","
                            (List.map (\p -> String.fromInt p.phaseId)
                                model.querySelectedPhases
                            )
                        )

        queryParams =
            Url.Builder.toQuery <|
                List.filterMap builder
                    [ ( "text"
                      , model.queryText
                      )
                    , ( "phases"
                      , phases
                      )
                    , ( "enrollment"
                      , enrollment
                      )
                    , ( "condition_names"
                      , model.queryConditions
                      )
                    , ( "sponsor_names"
                      , model.querySponsors
                      )
                    ]

        searchUrl =
            apiServer ++ "/search/" ++ queryParams
    in
    Http.get
        { url = searchUrl
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> SearchResponse)
                (Json.Decode.list decoderStudy)
        }


strToMaybe : String -> Maybe String
strToMaybe s =
    case String.length s of
        0 ->
            Nothing

        _ ->
            Just s


truncate : String -> Int -> String
truncate text max =
    case String.length text <= (max - 3) of
        True ->
            text

        _ ->
            String.left (max - 3) text ++ "..."


decoderCondition : Decoder Condition
decoderCondition =
    Json.Decode.succeed Condition
        |> Json.Decode.Pipeline.required "condition_id" int
        |> Json.Decode.Pipeline.required "condition_name" string
        |> Json.Decode.Pipeline.required "num_studies" int


decoderPhase : Decoder Phase
decoderPhase =
    Json.Decode.succeed Phase
        |> Json.Decode.Pipeline.required "phase_id" int
        |> Json.Decode.Pipeline.required "phase_name" string
        |> Json.Decode.Pipeline.required "num_studies" int


decoderSummary : Decoder Summary
decoderSummary =
    Json.Decode.succeed Summary
        |> Json.Decode.Pipeline.required "num_studies" int


decoderStudy : Decoder Study
decoderStudy =
    Json.Decode.succeed Study
        |> Json.Decode.Pipeline.required "study_id" int
        |> Json.Decode.Pipeline.required "nct_id" string
        |> Json.Decode.Pipeline.required "title" string
        |> Json.Decode.Pipeline.optional "detailed_description" string ""


decoderSponsor : Decoder Sponsor
decoderSponsor =
    Json.Decode.succeed Sponsor
        |> Json.Decode.Pipeline.required "sponsor_id" int
        |> Json.Decode.Pipeline.required "sponsor_name" string
        |> Json.Decode.Pipeline.required "num_studies" int


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
