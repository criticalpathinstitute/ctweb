module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

import Bool.Extra exposing (ifElse)
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
import Maybe.Extra exposing (isNothing, unwrap)
import Regex
import RemoteData exposing (RemoteData, WebData)
import Route
import Session exposing (Session, SessionUser(..))
import Set
import Task
import Types exposing (SearchParams)
import Url.Builder


type alias Model =
    { errorMessage : Maybe String
    , phases : WebData (List Phase)
    , initPhaseIds : Maybe (List Int)
    , initStudyTypeIds : Maybe (List Int)
    , queryConditions : Maybe String
    , queryConditionsBool : Bool
    , queryEnrollment : Maybe Int
    , queryInterventions : Maybe String
    , queryInterventionsBool : Bool
    , queryLastUpdatePosted : Maybe String
    , querySelectedPhases : List Phase
    , querySelectedStudyTypes : List StudyType
    , querySponsors : Maybe String
    , querySponsorsBool : Bool
    , queryStudyFirstPosted : Maybe String
    , queryText : Maybe String
    , queryTextBool : Bool
    , recordLimit : Int
    , searchName : Maybe String
    , searchResults : WebData SearchResults
    , selectedStudies : List Study
    , session : Session
    , studyTypes : WebData (List StudyType)
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
    }


type alias SavedSearches =
    { numSavedSearches : Int
    }


type alias Sponsor =
    { sponsorId : Int
    , sponsorName : String
    , numStudies : Int
    }


type alias StudyType =
    { studyTypeId : Int
    , studyTypeName : String
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


type alias SearchResults =
    { count : Int
    , records : List Study
    }


type Msg
    = AddPhase String
    | AddStudyType String
    | CartMsg Cart.Msg
    | DoSearch
    | PhasesResponse (WebData (List Phase))
    | RemovePhase Phase
    | RemoveStudyType StudyType
    | Reset
    | SaveSearch
    | SavedSearchResponse (WebData SavedSearches)
    | SearchResponse (WebData SearchResults)
    | SetConditions String
    | SetEnrollment String
    | SetInterventions String
    | SetLastUpdatePosted String
    | SetStudyFirstPosted String
    | SetQueryConditionsBool Bool
    | SetQueryInterventionsBool Bool
    | SetQuerySponsorsBool Bool
    | SetQueryText String
    | SetQueryTextBool Bool
    | SetRecordLimit String
    | SetSearchName String
    | SetSponsors String
    | StudyTypesResponse (WebData (List StudyType))



-- | SummaryResponse (WebData Summary)


defaultRecordLimit =
    100


initialModel : Session -> Model
initialModel session =
    { errorMessage = Nothing
    , initPhaseIds = Nothing
    , initStudyTypeIds = Nothing
    , phases = RemoteData.NotAsked
    , queryConditions = Nothing
    , queryConditionsBool = False
    , queryEnrollment = Nothing
    , queryInterventions = Nothing
    , queryInterventionsBool = False
    , queryLastUpdatePosted = Nothing
    , querySelectedPhases = []
    , querySelectedStudyTypes = []
    , querySponsors = Nothing
    , querySponsorsBool = False
    , queryStudyFirstPosted = Nothing
    , queryText = Nothing
    , queryTextBool = False
    , recordLimit = defaultRecordLimit
    , searchName = Nothing
    , searchResults = RemoteData.NotAsked
    , selectedStudies = []
    , session = session
    , studyTypes = RemoteData.NotAsked
    , summary = RemoteData.NotAsked
    }


init : Session -> Maybe SearchParams -> ( Model, Cmd Msg )
init session params =
    let
        _ =
            Debug.log "params" params

        model =
            initialModel session

        queryText =
            Maybe.andThen .fullText params

        queryTextBool =
            unwrap False .fullTextBool params

        queryConditions =
            Maybe.andThen .conditions params

        queryConditionsBool =
            unwrap False .conditionsBool params

        querySponsors =
            Maybe.andThen .sponsors params

        querySponsorsBool =
            unwrap False .sponsorsBool params

        queryLastUpdatePosted =
            Maybe.andThen .lastUpdatePosted params

        queryStudyFirstPosted =
            Maybe.andThen .studyFirstPosted params

        queryInterventions =
            Maybe.andThen .interventions params

        queryInterventionsBool =
            unwrap False .interventionsBool params

        queryEnrollment =
            Maybe.andThen (\p -> Just p.enrollment) params

        searchName =
            Maybe.andThen .searchName params

        initPhaseIds =
            Maybe.andThen (\p -> Just p.phaseIds) params

        initStudyTypeIds =
            Maybe.andThen (\p -> Just p.studyTypeIds) params
    in
    ( { model
        | initPhaseIds = initPhaseIds
        , initStudyTypeIds = initStudyTypeIds
        , queryConditions = queryConditions
        , queryConditionsBool = queryConditionsBool
        , queryEnrollment = queryEnrollment
        , queryInterventions = queryInterventions
        , queryInterventionsBool = queryInterventionsBool
        , queryLastUpdatePosted = queryLastUpdatePosted
        , querySponsors = querySponsors
        , querySponsorsBool = querySponsorsBool
        , queryStudyFirstPosted = queryStudyFirstPosted
        , queryText = queryText
        , queryTextBool = queryTextBool
        , searchName = searchName
      }
    , Cmd.batch [ getPhases, getStudyTypes ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPhase phaseId ->
            let
                currentPhaseIds =
                    Set.fromList <|
                        List.map (\p -> p.phaseId) model.querySelectedPhases

                newPhaseIds =
                    case String.toInt phaseId of
                        Just newId ->
                            Set.insert newId currentPhaseIds

                        _ ->
                            currentPhaseIds

                allPhases =
                    case model.phases of
                        RemoteData.Success data ->
                            data

                        _ ->
                            []

                newPhases =
                    List.filter (\p -> Set.member p.phaseId newPhaseIds)
                        allPhases

                newModel =
                    { model | querySelectedPhases = newPhases }
            in
            ( newModel, Cmd.none )

        AddStudyType studyTypeId ->
            let
                currentStudyTypeIds =
                    Set.fromList <|
                        List.map (\s -> s.studyTypeId)
                            model.querySelectedStudyTypes

                allStudyTypes =
                    case model.studyTypes of
                        RemoteData.Success data ->
                            data

                        _ ->
                            []

                newStudyTypeIds =
                    case String.toInt studyTypeId of
                        Just newId ->
                            Set.insert newId currentStudyTypeIds

                        _ ->
                            currentStudyTypeIds

                newStudyTypes =
                    List.filter
                        (\s -> Set.member s.studyTypeId newStudyTypeIds)
                        allStudyTypes

                newModel =
                    { model | querySelectedStudyTypes = newStudyTypes }
            in
            ( newModel, Cmd.none )

        CartMsg subMsg ->
            let
                cart =
                    model.session.cart

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

        PhasesResponse phases ->
            let
                newPhases =
                    case phases of
                        RemoteData.Success data ->
                            data

                        _ ->
                            []

                selectPhases initPhaseIds =
                    List.filter
                        (\p -> List.member p.phaseId initPhaseIds)
                        newPhases

                newSelectedPhases =
                    unwrap [] selectPhases model.initPhaseIds
            in
            ( { model
                | phases = phases
                , querySelectedPhases = newSelectedPhases
                , initPhaseIds = Nothing
              }
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

        RemoveStudyType newStudyType ->
            let
                newStudyTypes =
                    List.filter
                        (\studyType -> studyType /= newStudyType)
                        model.querySelectedStudyTypes

                newModel =
                    { model | querySelectedStudyTypes = newStudyTypes }
            in
            ( newModel, Cmd.none )

        Reset ->
            let
                newModel =
                    initialModel model.session
            in
            ( { newModel
                | phases = model.phases
                , studyTypes = model.studyTypes
              }
            , Cmd.none
            )

        SaveSearch ->
            ( model, saveSearch model )

        --SummaryResponse data ->
        --    ( { model | summary = data }
        --    , Cmd.none
        --    )
        SavedSearchResponse data ->
            ( model, Cmd.none )

        SearchResponse data ->
            ( { model | searchResults = data }
            , Cmd.none
            )

        SetConditions text ->
            ( { model | queryConditions = strToMaybe (String.toLower text) }
            , Cmd.none
            )

        SetInterventions text ->
            ( { model | queryInterventions = strToMaybe (String.toLower text) }
            , Cmd.none
            )

        SetLastUpdatePosted text ->
            ( { model | queryLastUpdatePosted = strToMaybe text }
            , Cmd.none
            )

        SetStudyFirstPosted text ->
            ( { model | queryStudyFirstPosted = strToMaybe text }
            , Cmd.none
            )

        SetQueryConditionsBool val ->
            ( { model | queryConditionsBool = val }, Cmd.none )

        SetSearchName text ->
            ( { model | searchName = strToMaybe text }
            , Cmd.none
            )

        SetSponsors text ->
            ( { model | querySponsors = strToMaybe (String.toLower text) }
            , Cmd.none
            )

        SetQueryInterventionsBool val ->
            ( { model | queryInterventionsBool = val }, Cmd.none )

        SetQuerySponsorsBool val ->
            ( { model | querySponsorsBool = val }, Cmd.none )

        SetQueryText query ->
            ( { model | queryText = strToMaybe query }, Cmd.none )

        SetQueryTextBool val ->
            ( { model | queryTextBool = val }, Cmd.none )

        SetRecordLimit val ->
            ( { model
                | recordLimit =
                    Maybe.withDefault defaultRecordLimit
                        (String.toInt val)
              }
            , Cmd.none
            )

        SetEnrollment enrollment ->
            ( { model | queryEnrollment = String.toInt enrollment }, Cmd.none )

        StudyTypesResponse studyTypes ->
            let
                newStudyTypes =
                    case studyTypes of
                        RemoteData.Success data ->
                            data

                        _ ->
                            []

                selectStudyTypes initStudyTypeIds =
                    List.filter
                        (\p -> List.member p.studyTypeId initStudyTypeIds)
                        newStudyTypes

                newSelectedStudyTypes =
                    unwrap [] selectStudyTypes model.initStudyTypeIds
            in
            ( { model
                | studyTypes = studyTypes
                , querySelectedStudyTypes = newSelectedStudyTypes
                , initStudyTypeIds = Nothing
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        empty =
            [ Select.item [ value "" ] [ text "--Select--" ] ]

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
                        [ text phase.phaseName
                        ]
            in
            case model.phases of
                RemoteData.Success data ->
                    mkSelect data

                RemoteData.Failure httpError ->
                    text (viewHttpErrorMessage httpError)

                _ ->
                    text "Loading phases..."

        mkStudyTypesSelect =
            let
                mkSelect data =
                    case List.length data of
                        0 ->
                            text "No study types"

                        _ ->
                            Select.select
                                [ Select.id "study_type"
                                , Select.onChange AddStudyType
                                ]
                                (empty ++ List.map mkSelectItem data)

                mkSelectItem studyType =
                    Select.item
                        [ value <| String.fromInt studyType.studyTypeId ]
                        [ text studyType.studyTypeName ]
            in
            case model.studyTypes of
                RemoteData.Success data ->
                    mkSelect data

                RemoteData.Failure httpError ->
                    text (viewHttpErrorMessage httpError)

                _ ->
                    text "Loading study types..."

        viewPhase phase =
            Button.button
                [ Button.outlinePrimary
                , Button.onClick (RemovePhase phase)
                ]
                [ text (phase.phaseName ++ " ⦻") ]

        viewStudyType studyType =
            Button.button
                [ Button.outlinePrimary
                , Button.onClick (RemoveStudyType studyType)
                ]
                [ text (studyType.studyTypeName ++ " ⦻") ]

        viewSelectedPhases =
            List.map viewPhase model.querySelectedPhases

        viewSelectedStudyTypes =
            List.map viewStudyType model.querySelectedStudyTypes

        isNotEmpty val =
            String.length (Maybe.withDefault "" val) > 0

        canSearch =
            List.member
                True
                [ isNotEmpty model.queryText
                , isNotEmpty model.queryConditions
                , isNotEmpty model.queryInterventions
                , isNotEmpty model.querySponsors
                , isNotEmpty model.queryLastUpdatePosted
                , isNotEmpty model.queryStudyFirstPosted
                , Maybe.withDefault 0 model.queryEnrollment > 0
                , List.length model.querySelectedPhases > 0
                , List.length model.querySelectedStudyTypes > 0
                ]

        enrollmentDisplay =
            let
                val =
                    Maybe.withDefault 0 model.queryEnrollment
            in
            if val > 0 then
                String.fromInt val

            else
                ""

        searchForm =
            Form.form [ onSubmit DoSearch ]
                [ Form.row []
                    [ Form.colLabel [ Col.sm2 ]
                        [ text "Full Text" ]
                    , Form.col [ Col.sm5 ]
                        [ Input.text
                            [ Input.attrs
                                [ onInput SetQueryText
                                , value <|
                                    Maybe.withDefault "" model.queryText
                                ]
                            ]
                        ]
                    , Form.col [ Col.sm2 ]
                        [ Checkbox.checkbox
                            [ Checkbox.id "chkTextBool"
                            , Checkbox.checked model.queryTextBool
                            , Checkbox.onCheck SetQueryTextBool
                            ]
                            "Boolean"
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ]
                        [ text "Conditions" ]
                    , Form.col [ Col.sm5 ]
                        [ Input.text
                            [ Input.attrs
                                [ onInput SetConditions
                                , value <|
                                    Maybe.withDefault "" model.queryConditions
                                ]
                            ]
                        ]
                    , Form.col [ Col.sm2 ]
                        [ Checkbox.checkbox
                            [ Checkbox.id "chkConditionsBool"
                            , Checkbox.checked model.queryConditionsBool
                            , Checkbox.onCheck SetQueryConditionsBool
                            ]
                            "Boolean"
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ]
                        [ text "Interventions" ]
                    , Form.col [ Col.sm5 ]
                        [ Input.text
                            [ Input.attrs
                                [ onInput SetInterventions
                                , value <|
                                    Maybe.withDefault ""
                                        model.queryInterventions
                                ]
                            ]
                        ]
                    , Form.col [ Col.sm2 ]
                        [ Checkbox.checkbox
                            [ Checkbox.id "chkInterventionsBool"
                            , Checkbox.checked model.queryInterventionsBool
                            , Checkbox.onCheck SetQueryInterventionsBool
                            ]
                            "Boolean"
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ]
                        [ text "Sponsors" ]
                    , Form.col [ Col.sm5 ]
                        [ Input.text
                            [ Input.attrs
                                [ onInput SetSponsors
                                , value <|
                                    Maybe.withDefault "" model.querySponsors
                                ]
                            ]
                        ]
                    , Form.col [ Col.sm2 ]
                        [ Checkbox.checkbox
                            [ Checkbox.id "chkSponsorsBool"
                            , Checkbox.checked model.querySponsorsBool
                            , Checkbox.onCheck SetQuerySponsorsBool
                            ]
                            "Boolean"
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ] [ text "Phases" ]
                    , Form.col [ Col.sm5 ]
                        [ mkPhasesSelect
                        , div [] viewSelectedPhases
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ] [ text "Study Type" ]
                    , Form.col [ Col.sm5 ]
                        [ mkStudyTypesSelect
                        , div [] viewSelectedStudyTypes
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ] [ text "Enrollment" ]
                    , Form.col [ Col.sm5 ]
                        [ Input.text
                            [ Input.attrs
                                [ onInput SetEnrollment
                                , value enrollmentDisplay
                                ]
                            ]
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ] [ text "Study First Posted" ]
                    , Form.col [ Col.sm5 ]
                        [ Input.text
                            [ Input.attrs
                                [ onInput SetStudyFirstPosted
                                , value
                                    (Maybe.withDefault
                                        ""
                                        model.queryStudyFirstPosted
                                    )
                                ]
                            ]
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ] [ text "Last Update Posted" ]
                    , Form.col [ Col.sm5 ]
                        [ Input.text
                            [ Input.attrs
                                [ onInput SetLastUpdatePosted
                                , value
                                    (Maybe.withDefault
                                        ""
                                        model.queryLastUpdatePosted
                                    )
                                ]
                            ]
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ] [ text "Limit" ]
                    , Form.col [ Col.sm5 ]
                        [ Input.text
                            [ Input.attrs
                                [ onInput SetRecordLimit
                                , value (String.fromInt model.recordLimit)
                                ]
                            ]
                        ]
                    ]
                , Form.row []
                    [ Form.colLabel [ Col.sm2 ] [ text "Search Name" ]
                    , Form.col [ Col.sm5 ]
                        [ Input.text
                            [ Input.attrs
                                [ onInput SetSearchName
                                , value (Maybe.withDefault "" model.searchName)
                                ]
                            ]
                        ]
                    ]
                , Form.row []
                    [ Form.col [ Col.offsetSm2, Col.sm7 ]
                        [ Button.button
                            [ Button.primary
                            , Button.onClick DoSearch
                            , Button.attrs [ Spacing.mx1 ]
                            , Button.disabled (not canSearch)
                            ]
                            [ text "Submit" ]
                        , Button.button
                            [ Button.secondary
                            , Button.onClick Reset
                            , Button.attrs [ Spacing.mx1 ]
                            ]
                            [ text "Clear" ]
                        , Button.button
                            [ Button.secondary
                            , Button.onClick SaveSearch
                            , Button.attrs [ Spacing.mx1 ]
                            , Button.disabled <|
                                Bool.Extra.any
                                    [ not canSearch, isNothing model.searchName ]
                            ]
                            [ text "Save Search" ]
                        ]
                    ]
                ]

        viewSearchResults =
            case model.searchResults of
                RemoteData.NotAsked ->
                    div [] [ text "" ]

                RemoteData.Loading ->
                    div [] [ img [ src "/assets/images/loading.gif" ] [] ]

                RemoteData.Failure httpError ->
                    div [] [ text (viewHttpErrorMessage httpError) ]

                RemoteData.Success data ->
                    let
                        cart =
                            model.session.cart

                        title =
                            "Showing "
                                ++ commify (List.length data.records)
                                ++ " of "
                                ++ commify data.count
                                ++ " found"

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
                                            data.records
                                        )
                                }

                        resultsDiv =
                            let
                                idList =
                                    List.map (\s -> s.studyId) data.records

                                cartButton =
                                    div []
                                        [ Cart.addAllToCartButton cart idList
                                            |> Html.map CartMsg
                                        ]

                                body =
                                    case data.count of
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
        --[ text summary
        --, searchForm
        --]
        [ Grid.row []
            [ Grid.col [] [ searchForm ] ]
        , Grid.row []
            [ Grid.col [] [ viewSearchResults ] ]
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


getPhases : Cmd Msg
getPhases =
    Http.get
        { url = apiServer ++ "/phases"
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> PhasesResponse)
                (Json.Decode.list decoderPhase)
        }


getStudyTypes : Cmd Msg
getStudyTypes =
    Http.get
        { url = apiServer ++ "/study_types"
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> StudyTypesResponse)
                (Json.Decode.list decoderStudyType)
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

        phaseIds =
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

        studyTypeIds =
            case List.length model.querySelectedStudyTypes of
                0 ->
                    Nothing

                _ ->
                    Just
                        (String.join ","
                            (List.map (\s -> String.fromInt s.studyTypeId)
                                model.querySelectedStudyTypes
                            )
                        )

        queryTextBool =
            Url.Builder.int "text_bool" (ifElse 1 0 model.queryTextBool)

        queryConditionsBool =
            Url.Builder.int "conditions_bool"
                (ifElse 1 0 model.queryConditionsBool)

        queryInterventionsBool =
            Url.Builder.int "interventions_bool"
                (ifElse 1 0 model.queryInterventionsBool)

        querySponsorsBool =
            Url.Builder.int "sponsors_bool"
                (ifElse 1 0 model.querySponsorsBool)

        recordLimit =
            Url.Builder.int "limit" model.recordLimit

        queryParams =
            List.filterMap builder
                [ ( "text"
                  , model.queryText
                  )
                , ( "phase_ids"
                  , phaseIds
                  )
                , ( "study_type_ids"
                  , studyTypeIds
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
                , ( "intervention_names"
                  , model.queryInterventions
                  )
                , ( "last_update_posted"
                  , model.queryLastUpdatePosted
                  )
                , ( "study_first_posted"
                  , model.queryStudyFirstPosted
                  )
                ]

        params =
            Url.Builder.toQuery <|
                queryParams
                    ++ [ queryTextBool
                       , queryConditionsBool
                       , queryInterventionsBool
                       , querySponsorsBool
                       , recordLimit
                       ]

        searchUrl =
            apiServer ++ "/search" ++ params
    in
    Http.get
        { url = searchUrl
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> SearchResponse)
                decoderSearchResults
        }


saveSearch : Model -> Cmd Msg
saveSearch model =
    case model.session.user of
        Guest ->
            Cmd.none

        LoggedIn user ->
            let
                emailId =
                    Url.Builder.string "email_id" user.email

                searchName =
                    Url.Builder.string "search_name"
                        (Maybe.withDefault "" model.searchName)

                fullText =
                    Url.Builder.string "full_text"
                        (Maybe.withDefault "" model.queryText)

                fullTextBool =
                    Url.Builder.int "full_text_bool"
                        (ifElse 1 0 model.queryTextBool)

                conditions =
                    Url.Builder.string "conditions"
                        (Maybe.withDefault "" model.queryConditions)

                conditionsBool =
                    Url.Builder.int "conditions_bool"
                        (ifElse 1 0 model.queryConditionsBool)

                sponsors =
                    Url.Builder.string "sponsors"
                        (Maybe.withDefault "" model.querySponsors)

                sponsorsBool =
                    Url.Builder.int "sponsors_bool"
                        (ifElse 1 0 model.querySponsorsBool)

                interventions =
                    Url.Builder.string "interventions"
                        (Maybe.withDefault "" model.queryInterventions)

                interventionsBool =
                    Url.Builder.int "interventions_bool"
                        (ifElse 1 0 model.queryInterventionsBool)

                phaseIds =
                    List.map (\p -> String.fromInt p.phaseId)
                        model.querySelectedPhases
                        |> String.join ","
                        |> Url.Builder.string "phase_ids"

                studyTypeIds =
                    List.map (\s -> String.fromInt s.studyTypeId)
                        model.querySelectedStudyTypes
                        |> String.join ","
                        |> Url.Builder.string "study_type_ids"

                enrollment =
                    Url.Builder.int "enrollment"
                        (Maybe.withDefault 0 model.queryEnrollment)

                email_to =
                    Url.Builder.string "email_to" user.email

                params =
                    Url.Builder.toQuery
                        [ emailId
                        , searchName
                        , fullText
                        , fullTextBool
                        , conditions
                        , conditionsBool
                        , sponsors
                        , sponsorsBool
                        , interventions
                        , interventionsBool
                        , phaseIds
                        , studyTypeIds
                        , enrollment
                        ]

                saveSearchUrl =
                    apiServer ++ "/save_search" ++ params
            in
            Http.get
                { url = saveSearchUrl
                , expect =
                    Http.expectJson
                        (RemoteData.fromResult >> SavedSearchResponse)
                        decoderSavedSearches
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



--decoderSummary : Decoder Summary
--decoderSummary =
--    Json.Decode.succeed Summary
--        |> Json.Decode.Pipeline.required "num_studies" int


decoderSavedSearches : Decoder SavedSearches
decoderSavedSearches =
    Json.Decode.succeed SavedSearches
        |> Json.Decode.Pipeline.required "num_save_searches" int


decoderSearchResults : Decoder SearchResults
decoderSearchResults =
    Json.Decode.succeed SearchResults
        |> Json.Decode.Pipeline.required "count" int
        |> Json.Decode.Pipeline.required "records"
            (Json.Decode.list decoderStudy)


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


decoderStudyType : Decoder StudyType
decoderStudyType =
    Json.Decode.succeed StudyType
        |> Json.Decode.Pipeline.required "study_type_id" int
        |> Json.Decode.Pipeline.required "study_type_name" string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
