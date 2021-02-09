module Page.Cart exposing (ExternalMsg(..), Model, Msg(..), init, toSession, update, view)

import Bootstrap.Button as Button
import Bootstrap.Table exposing (table, tbody, td, th, thead, tr)
import Bootstrap.Utilities.Spacing as Spacing
import Cart exposing (Cart)
import Config exposing (apiServer)
import File.Download as Download
import Html exposing (Html, a, b, br, div, h1, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Icon
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    , studies : RemoteData Http.Error (List Study)
    }


type alias Study =
    { studyId : Int
    , nctId : String
    , title : String
    }


init :
    Session
    -> ( Model, Cmd Msg ) --Maybe Int -> ( Model, Cmd Msg )
init session =
    let
        idList =
            Cart.toList (Session.getCart session)
    in
    ( { session = session
      , studies = Loading
      }
    , Cmd.batch [ getStudies idList ]
    )


toSession : Model -> Session
toSession model =
    model.session


type Msg
    = CartMsg Cart.Msg
    | DownloadStudies
    | EmptyCart
    | StudiesResponse (WebData (List Study))


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

                url =
                    apiServer ++ "/download?study_ids=" ++ studyIds
            in
            ( model, Download.url url )

        EmptyCart ->
            let
                newSession =
                    Session.setCart model.session Cart.empty
            in
            -- ( { model | session = newSession, files = Success [] }
            ( { model | session = newSession }
            , Cart.store Cart.empty
            )

        StudiesResponse data ->
            ( { model | studies = data }
            , Cmd.none
            )


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

                _ ->
                    text "?"

        cartSize =
            Cart.size cart
    in
    div []
        [ div []
            [ h1 [] [ text <| "View Cart (" ++ String.fromInt cartSize ++ ")" ]
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



--view : Model -> Html Msg
--view model =
--    let
--        isLoggedIn =
--            False
--        --model.userId /= Nothing
--        cart =
--            Session.getCart model.session
--        count =
--            Cart.size cart
--        isEmpty =
--            count == 0
--    in
--    Page.viewRemoteData model.files
--        (\files ->
--            div [ class "container" ]
--                [ div [ class "pb-2 mt-5 mb-2" ]
--                    [ h1 [ class "font-weight-bold align-middle d-inline" ]
--                        [ span [ style "color" "dimgray" ] [ text "Cart" ]
--                        ]
--                    , span [ class "badge badge-pill badge-primary ml-2" ]
--                        [ if count == 0 then
--                            text ""
--                          else
--                            text (String.fromInt count)
--                        ]
--                    , button [ class "btn btn-primary mt-2 float-right", onClick EmptyCart, disabled isEmpty ]
--                        [ Icon.ban, text " Empty" ]
--                    ]
--                , viewCart cart files
--                ]
--        )
--viewCartControls :
--    Bool
--    -> Bool
--    -> Html Msg -- -> Maybe Int -> List SampleGroup -> Html Msg
--viewCartControls isEmpty isLoggedIn =
--    -- selectedCartId sampleGroups =
--    button [ class "btn btn-primary", onClick EmptyCart, disabled isEmpty ]
--        [ Icon.ban, text " Empty" ]
--viewCart : Cart -> List File -> Html Msg
--viewCart cart files =
--    if Cart.size cart == 0 then
--        div [ class "alert alert-secondary" ] [ text "The cart is empty" ]
--    else
--        Cart.view cart files Cart.Editable |> Html.map CartMsg
