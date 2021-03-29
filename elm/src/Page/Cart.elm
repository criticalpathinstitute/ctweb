module Page.Cart exposing (ExternalMsg(..), Model, Msg(..), init, toSession, update, view)

import Bootstrap.Button as Button
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Tab as Tab
import Bootstrap.Table exposing (table, tbody, td, th, thead, tr)
import Bootstrap.Utilities.Spacing as Spacing
import Cart exposing (Cart)
import Common exposing (commify, viewHttpErrorMessage)
import Config exposing (apiServer)
import File.Download as Download
import Html exposing (Html, a, b, br, div, h1, img, text)
import Html.Attributes exposing (src, target)
import Html.Events exposing (onClick, onInput)
import Http
import Icon
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Set exposing (Set)


type alias Model =
    { session : Session
    , studies : RemoteData Http.Error (List Study)
    , tabState : Tab.State
    , downloadFields : Set String
    }


type alias Study =
    { studyId : Int
    , nctId : String
    , title : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        idList =
            Cart.toList (Session.getCart session)

        ( cmds, studies ) =
            if List.length idList > 0 then
                ( Cmd.batch [ getStudies idList ], RemoteData.Loading )

            else
                ( Cmd.none, RemoteData.NotAsked )
    in
    ( { session = session
      , studies = studies
      , tabState = Tab.initialState
      , downloadFields = Set.fromList allDownloadFields
      }
    , cmds
    )


allDownloadFields : List String
allDownloadFields =
    [ "nct_id"
    , "official_title"
    , "brief_title"
    , "overall_status"
    , "last_known_status"
    , "brief_summary"
    , "detailed_description"
    , "keywords"
    , "enrollment"
    , "start_date"
    , "completion_date"
    ]


toSession : Model -> Session
toSession model =
    model.session


type Msg
    = CartMsg Cart.Msg
    | DownloadStudies
    | EmptyCart
    | StudiesResponse (WebData (List Study))
    | TabMsg Tab.State
    | SetFldNctId Bool
    | SetFldOfficialTitle Bool
    | SetFldBriefTitle Bool
    | SetFldOverallStatus Bool
    | SetFldLastKnownStatus Bool
    | SetFldBriefSummary Bool
    | SetFldDetailedDescription Bool
    | SetFldKeywords Bool
    | SetFldEnrollment Bool
    | SetFldStartDate Bool
    | SetFldCompletionDate Bool
    | SelectAllDownloadFields
    | UnselectAllDownloadFields


type ExternalMsg
    = NoOp
    | SetCart Cart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CartMsg subMsg ->
            let
                newCart =
                    Cart.update subMsg (Session.getCart model.session)

                newSession =
                    Session.setCart model.session newCart
            in
            ( { model | session = newSession }
            , Cmd.batch [ Cart.store newCart ]
            )

        DownloadStudies ->
            let
                idList =
                    Cart.toList (Session.getCart model.session)

                studyIds =
                    String.join "," <|
                        List.map String.fromInt idList

                selectedFields =
                    List.filter (\f -> Set.member f model.downloadFields)
                        allDownloadFields

                url =
                    apiServer
                        ++ "/download?study_ids="
                        ++ studyIds
                        ++ "&fields="
                        ++ String.join "," selectedFields
            in
            ( model, Download.url url )

        EmptyCart ->
            let
                newSession =
                    Session.setCart model.session Cart.empty
            in
            ( { model | session = newSession }
            , Cart.store Cart.empty
            )

        SetFldNctId toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle "nct_id" model.downloadFields
              }
            , Cmd.none
            )

        SetFldOfficialTitle toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "official_title"
                        model.downloadFields
              }
            , Cmd.none
            )

        SetFldBriefTitle toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "brief_title"
                        model.downloadFields
              }
            , Cmd.none
            )

        SetFldOverallStatus toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "overall_status"
                        model.downloadFields
              }
            , Cmd.none
            )

        SetFldLastKnownStatus toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "last_known_status"
                        model.downloadFields
              }
            , Cmd.none
            )

        SetFldBriefSummary toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "brief_summary"
                        model.downloadFields
              }
            , Cmd.none
            )

        SetFldDetailedDescription toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "detailed_description"
                        model.downloadFields
              }
            , Cmd.none
            )

        SetFldKeywords toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "keywords"
                        model.downloadFields
              }
            , Cmd.none
            )

        SetFldEnrollment toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "enrollment"
                        model.downloadFields
              }
            , Cmd.none
            )

        SetFldStartDate toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "start_date"
                        model.downloadFields
              }
            , Cmd.none
            )

        SetFldCompletionDate toggle ->
            ( { model
                | downloadFields =
                    toggleDownloadField toggle
                        "completion_date"
                        model.downloadFields
              }
            , Cmd.none
            )

        StudiesResponse data ->
            ( { model | studies = data }
            , Cmd.none
            )

        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )

        UnselectAllDownloadFields ->
            ( { model | downloadFields = Set.empty }
            , Cmd.none
            )

        SelectAllDownloadFields ->
            ( { model | downloadFields = Set.fromList allDownloadFields }
            , Cmd.none
            )


toggleDownloadField : Bool -> String -> Set String -> Set String
toggleDownloadField toggleOn field fields =
    if toggleOn then
        Set.insert field fields

    else
        Set.remove field fields


view : Model -> Html Msg
view model =
    let
        cart =
            Session.getCart model.session

        studies =
            case model.studies of
                RemoteData.Success data ->
                    let
                        mkRow study =
                            tr []
                                [ td []
                                    [ Cart.addToCartButton cart study.studyId
                                        |> Html.map CartMsg
                                    ]
                                , td []
                                    [ b [] [ text study.title ]
                                    , br [] []
                                    , a
                                        [ Route.href
                                            (Route.Study study.nctId)
                                        , target "_blank"
                                        ]
                                        [ text study.nctId ]
                                    ]
                                ]
                    in
                    table
                        { options =
                            [ Bootstrap.Table.striped ]
                        , thead = thead [] []
                        , tbody =
                            tbody []
                                (List.map mkRow data)
                        }

                RemoteData.Loading ->
                    div [] [ img [ src "/assets/images/loading.gif" ] [] ]

                RemoteData.NotAsked ->
                    div [] [ text "Not asked" ]

                RemoteData.Failure httpError ->
                    div [] [ text <| viewHttpErrorMessage httpError ]

        cartSize =
            Cart.size cart

        cartView =
            div []
                [ div []
                    [ h1 []
                        [ text <|
                            "View Cart ("
                                ++ String.fromInt cartSize
                                ++ ")"
                        ]
                    , Button.button
                        [ Button.outlinePrimary
                        , Button.onClick DownloadStudies
                        , Button.disabled (cartSize == 0)
                        , Button.attrs [ Spacing.mx1 ]
                        ]
                        [ text "Download" ]
                    , Button.button
                        [ Button.outlinePrimary
                        , Button.onClick EmptyCart
                        , Button.disabled (cartSize == 0)
                        , Button.attrs [ Spacing.mx1 ]
                        ]
                        [ text "Empty Cart" ]
                    ]
                , studies
                ]

        tabs =
            Tab.config TabMsg
                |> Tab.items
                    [ Tab.item
                        { id = "tabCart"
                        , link = Tab.link [] [ text "Contents" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ]
                                [ cartView ]
                        }
                    , Tab.item
                        { id = "tabDownloadFields"
                        , link = Tab.link [] [ text "Download Fields" ]
                        , pane =
                            Tab.pane [ Spacing.mt3 ] [ downloadConfig model ]
                        }
                    ]
                |> Tab.view model.tabState
    in
    Grid.container []
        [ Grid.row []
            [ Grid.col [] [ tabs ]
            ]
        ]


downloadConfig : Model -> Html Msg
downloadConfig model =
    div []
        [ Button.button
            [ Button.outlinePrimary
            , Button.onClick SelectAllDownloadFields
            , Button.attrs [ Spacing.mx1 ]
            ]
            [ text "Select All" ]
        , Button.button
            [ Button.outlinePrimary
            , Button.onClick UnselectAllDownloadFields
            , Button.attrs [ Spacing.mx1 ]
            ]
            [ text "Unselect All" ]
        , Checkbox.checkbox
            [ Checkbox.id "chkNctId"
            , Checkbox.checked (Set.member "nct_id" model.downloadFields)
            , Checkbox.onCheck SetFldNctId
            ]
            "NCT ID"
        , Checkbox.checkbox
            [ Checkbox.id "chkOfficialTitle"
            , Checkbox.checked
                (Set.member "official_title" model.downloadFields)
            , Checkbox.onCheck SetFldOfficialTitle
            ]
            "Official Title"
        , Checkbox.checkbox
            [ Checkbox.id "chkBriefTitle"
            , Checkbox.checked (Set.member "brief_title" model.downloadFields)
            , Checkbox.onCheck SetFldBriefTitle
            ]
            "Brief Title"
        , Checkbox.checkbox
            [ Checkbox.id "chbOverallStatus"
            , Checkbox.checked
                (Set.member "overall_status" model.downloadFields)
            , Checkbox.onCheck SetFldOverallStatus
            ]
            "Overall Status"
        , Checkbox.checkbox
            [ Checkbox.id "chbLastKnownStatus"
            , Checkbox.checked
                (Set.member "last_known_status" model.downloadFields)
            , Checkbox.onCheck SetFldLastKnownStatus
            ]
            "Last Known Status"
        , Checkbox.checkbox
            [ Checkbox.id "chbBriefSummary"
            , Checkbox.checked
                (Set.member "brief_summary" model.downloadFields)
            , Checkbox.onCheck SetFldBriefSummary
            ]
            "Brief Summary"
        , Checkbox.checkbox
            [ Checkbox.id "chbDetailedDescription"
            , Checkbox.checked
                (Set.member "detailed_description" model.downloadFields)
            , Checkbox.onCheck SetFldDetailedDescription
            ]
            "Detailed Description"
        , Checkbox.checkbox
            [ Checkbox.id "chbKeywords"
            , Checkbox.checked (Set.member "keywords" model.downloadFields)
            , Checkbox.onCheck SetFldKeywords
            ]
            "Keywords"
        , Checkbox.checkbox
            [ Checkbox.id "chbEnrollment"
            , Checkbox.checked (Set.member "enrollment" model.downloadFields)
            , Checkbox.onCheck SetFldEnrollment
            ]
            "Enrollment"
        , Checkbox.checkbox
            [ Checkbox.id "chbStartDate"
            , Checkbox.checked (Set.member "start_date" model.downloadFields)
            , Checkbox.onCheck SetFldStartDate
            ]
            "Start Date"
        , Checkbox.checkbox
            [ Checkbox.id "chbCompletionDate"
            , Checkbox.checked
                (Set.member "completion_date" model.downloadFields)
            , Checkbox.onCheck SetFldCompletionDate
            ]
            "Completion Date"
        ]


getStudies : List Int -> Cmd Msg
getStudies studyIds =
    let
        ids =
            String.join "," (List.map String.fromInt studyIds)
    in
    Http.get
        { url = apiServer ++ "/view_cart?study_ids=" ++ ids
        , expect =
            Http.expectJson
                (RemoteData.fromResult >> StudiesResponse)
                (Json.Decode.list decoderStudy)
        }


decoderStudy : Decoder Study
decoderStudy =
    Json.Decode.succeed Study
        |> Json.Decode.Pipeline.required "study_id" int
        |> Json.Decode.Pipeline.required "nct_id" string
        |> Json.Decode.Pipeline.required "title" string
