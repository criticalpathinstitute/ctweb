module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Nav
import Cart as CartData
import Config
import Credentials exposing (Credentials)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline
import Json.Encode as Encode exposing (Value)
import Page.Cart
import Page.Conditions
import Page.Home
import Page.Sponsors
import Page.Study
import PageView
import Route exposing (Route)
import Session exposing (Session)
import State exposing (State)
import Url


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , curPage : Page
    , navbarState : Navbar.State
    , session : Session
    }


type alias Flags =
    { cart : Maybe CartData.Cart
    , cred : Maybe Credentials
    }


type Page
    = CartPage Page.Cart.Model
    | ConditionsPage Page.Conditions.Model
    | HomePage Page.Home.Model
    | StudyPage String Page.Study.Model
    | SponsorsPage Page.Sponsors.Model


type Msg
    = CartMsg Page.Cart.Msg
    | ConditionsMsg Page.Conditions.Msg
    | HomeMsg Page.Home.Msg
    | LinkClicked Browser.UrlRequest
    | NavbarMsg Navbar.State
    | StudyMsg Page.Study.Msg
    | SponsorsMsg Page.Sponsors.Msg
    | UrlChanged Url.Url


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> Json.Decode.Pipeline.required "cart"
            (Decode.nullable CartData.decoder)
        |> Json.Decode.Pipeline.required "cred"
            (Decode.nullable Credentials.decoder)


init : Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        session =
            case Decode.decodeValue flagsDecoder flags of
                Ok f ->
                    let
                        cart =
                            f.cart |> Maybe.withDefault CartData.empty
                    in
                    case f.cred of
                        Just cred ->
                            Session.LoggedIn navKey State.default cart cred

                        Nothing ->
                            Session.Guest navKey State.default cart

                Err error ->
                    Session.Guest navKey State.default CartData.empty

        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        ( homeModel, homeMsg ) =
            Page.Home.init session Nothing

        initialModel =
            { key = navKey
            , url = url
            , curPage = HomePage homeModel
            , navbarState = navbarState
            , session = session
            }

        currentRoute =
            Route.fromUrl url

        ( newModel, subMsg ) =
            changeRouteTo currentRoute initialModel
    in
    ( newModel, Cmd.batch [ navbarCmd, subMsg ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.curPage ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( NavbarMsg state, _ ) ->
            ( { model | navbarState = state }, Cmd.none )

        ( CartMsg subMsg, CartPage subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.Cart.update subMsg subModel
            in
            ( { model
                | curPage = CartPage newSubModel
                , session = newSubModel.session
              }
            , Cmd.map CartMsg newCmd
            )

        ( ConditionsMsg subMsg, ConditionsPage subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.Conditions.update subMsg subModel
            in
            ( { model
                | curPage = ConditionsPage newSubModel
                , session = newSubModel.session
              }
            , Cmd.map ConditionsMsg newCmd
            )

        ( HomeMsg subMsg, HomePage subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.Home.update subMsg subModel
            in
            ( { model
                | curPage = HomePage newSubModel
                , session = newSubModel.session
              }
            , Cmd.map HomeMsg newCmd
            )

        ( StudyMsg subMsg, StudyPage nctId subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.Study.update subMsg subModel
            in
            ( { model
                | curPage = StudyPage nctId newSubModel
                , session = newSubModel.session
              }
            , Cmd.map StudyMsg newCmd
            )

        ( SponsorsMsg subMsg, SponsorsPage subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.Sponsors.update subMsg subModel
            in
            ( { model
                | curPage = SponsorsPage newSubModel
                , session = newSubModel.session
              }
            , Cmd.map SponsorsMsg newCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        navConfig =
            Navbar.config NavbarMsg
    in
    case model.curPage of
        CartPage subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map CartMsg (Page.Cart.view subModel))

        ConditionsPage subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map ConditionsMsg (Page.Conditions.view subModel))

        HomePage subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map HomeMsg (Page.Home.view subModel))

        StudyPage nctId subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map StudyMsg
                    (Page.Study.view subModel)
                )

        SponsorsPage subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map SponsorsMsg (Page.Sponsors.view subModel))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.curPage of
        HomePage subModel ->
            Sub.map HomeMsg
                (Page.Home.subscriptions subModel)

        _ ->
            Sub.none


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Just (Route.Study nctId) ->
            let
                ( subModel, subMsg ) =
                    Page.Study.init model.session nctId
            in
            ( { model | curPage = StudyPage nctId subModel }
            , Cmd.map StudyMsg subMsg
            )

        Just Route.Cart ->
            let
                ( subModel, subMsg ) =
                    Page.Cart.init model.session
            in
            ( { model | curPage = CartPage subModel }
            , Cmd.map CartMsg subMsg
            )

        Just Route.Conditions ->
            let
                ( subModel, subMsg ) =
                    Page.Conditions.init model.session
            in
            ( { model | curPage = ConditionsPage subModel }
            , Cmd.map ConditionsMsg subMsg
            )

        Just (Route.Home queryString) ->
            let
                ( subModel, subMsg ) =
                    Page.Home.init model.session queryString
            in
            ( { model | curPage = HomePage subModel }, Cmd.map HomeMsg subMsg )

        Just Route.Sponsors ->
            let
                ( subModel, subMsg ) =
                    Page.Sponsors.init model.session
            in
            ( { model | curPage = SponsorsPage subModel }
            , Cmd.map SponsorsMsg subMsg
            )

        _ ->
            let
                ( subModel, subMsg ) =
                    Page.Home.init model.session Nothing
            in
            ( { model | curPage = HomePage subModel }, Cmd.map HomeMsg subMsg )
