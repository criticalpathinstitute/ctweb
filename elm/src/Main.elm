module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation as Nav
import Cart as CartData
import Config
import Credentials exposing (Credentials)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline
import Json.Encode as Encode exposing (Value)
import OAuth
import OAuth.Implicit as OAuth
import Page.About
import Page.Cart
import Page.Conditions
import Page.Home
import Page.Profile
import Page.SignIn
import Page.Sponsors
import Page.Study
import PageView
import Route exposing (Route)
import Session exposing (Session, SessionUser(..))
import State exposing (State)
import Types exposing (Flow(..), FlowError(..), SearchParams)
import Url exposing (Url)
import User exposing (User)


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
    , flag : Int
    , searchParams : Maybe SearchParams
    , flow : Flow
    , redirectUrl : Maybe Url
    }


type alias Flags =
    { cart : Maybe CartData.Cart
    , cred : Maybe Credentials
    , bytes : Maybe Page.SignIn.State
    }


type Page
    = AboutPage Page.About.Model
    | CartPage Page.Cart.Model
    | ConditionsPage Page.Conditions.Model
    | HomePage Page.Home.Model
    | ProfilePage Page.Profile.Model
    | SignInPage Page.SignIn.Model
    | StudyPage String Page.Study.Model
    | SponsorsPage Page.Sponsors.Model


type Msg
    = AboutMsg Page.About.Msg
    | CartMsg Page.Cart.Msg
    | ConditionsMsg Page.Conditions.Msg
    | GotUserInfo (Result Http.Error User)
    | HomeMsg Page.Home.Msg
    | LinkClicked Browser.UrlRequest
    | Logout
    | NavbarMsg Navbar.State
    | ProfileMsg Page.Profile.InternalMsg
    | SignInMsg Page.SignIn.Msg
    | SetSearchParams SearchParams
    | SponsorsMsg Page.Sponsors.Msg
    | StudyMsg Page.Study.Msg
    | UrlChanged Url.Url
    | UserInfoRequested


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> Json.Decode.Pipeline.required "cart"
            (Decode.nullable CartData.decoder)
        |> Json.Decode.Pipeline.required "cred"
            (Decode.nullable Credentials.decoder)
        |> Json.Decode.Pipeline.required "bytes"
            (Decode.nullable Page.SignIn.decoder)


init : Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        currentRoute =
            Route.fromUrl url

        ( newPage, subMsg ) =
            changeRouteTo currentRoute session Nothing

        redirectUrl =
            { url | query = Nothing, fragment = Nothing }

        clearUrl =
            Nav.replaceUrl navKey (Url.toString url)

        ( session, bytes ) =
            case Decode.decodeValue flagsDecoder flags of
                Ok f ->
                    let
                        ( storedCred, storedUser ) =
                            case f.cred of
                                Just c ->
                                    let
                                        user =
                                            case c.user of
                                                Just u ->
                                                    Session.LoggedIn u

                                                _ ->
                                                    Session.Guest
                                    in
                                    ( Just c, user )

                                _ ->
                                    ( Nothing, Session.Guest )
                    in
                    ( Session
                        navKey
                        State.default
                        (Maybe.withDefault CartData.empty f.cart)
                        Idle
                        (Just
                            (Maybe.withDefault Credentials.default storedCred)
                        )
                        storedUser
                    , f.bytes
                    )

                _ ->
                    ( Session
                        navKey
                        State.default
                        CartData.empty
                        Idle
                        (Just Credentials.default)
                        Session.Guest
                    , Nothing
                    )

        ( newFlow, flowRedirectUri, cmd ) =
            case OAuth.parseToken url of
                OAuth.Empty ->
                    ( Idle, redirectUrl, clearUrl )

                OAuth.Success { token, state } ->
                    case bytes of
                        Nothing ->
                            ( Errored ErrStateMismatch, redirectUrl, clearUrl )

                        Just b ->
                            if state /= Just b.state then
                                ( Errored ErrStateMismatch
                                , redirectUrl
                                , clearUrl
                                )

                            else
                                ( Authorized token
                                , redirectUrl
                                , getUserInfo Page.SignIn.configuration token
                                )

                OAuth.Error error ->
                    ( Errored <| ErrAuthorization error
                    , redirectUrl
                    , clearUrl
                    )

        model =
            { key = navKey
            , url = url
            , curPage = newPage
            , navbarState = navbarState
            , session = { session | flow = newFlow }
            , flag = 0
            , searchParams = Nothing
            , flow = newFlow
            , redirectUrl = Nothing
            }
    in
    ( model, Cmd.batch [ navbarCmd, subMsg, cmd ] )



--case OAuth.parseToken url of
--    OAuth.Empty ->
--        ( { model | flow = Idle, redirectUrl = Just redirectUrl }
--        , Cmd.batch [ navbarCmd, subMsg ]
--        )
--    OAuth.Success { token, state } ->
--        --let
--        --_ =
--        --    Debug.log "token" token
--        --_ =
--        --    Debug.log "state" state
--        --_ =
--        --    Debug.log "bytes"
--        --newSession =
--        --    Session.setState session (State (Url.toString stateUrl))
--        --in
--        --( { model | curPage = Redirect newSession }
--        --, Cmd.batch
--        --    [ -- getAccessToken "agave" code |> Http.send GotAccessToken
--        --      navbarCmd
--        --    , subMsg
--        --    ]
--        --)
--        ( model, Cmd.batch [ navbarCmd, subMsg ] )
--    OAuth.Error error ->
--        ( { model
--            | flow = Errored <| ErrAuthorization error
--            , redirectUrl = Just redirectUrl
--          }
--        , Cmd.batch [ clearUrl, navbarCmd, subMsg ]
--        )


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
            let
                ( newPage, newMsg ) =
                    changeRouteTo (Route.fromUrl url)
                        model.session
                        model.searchParams
            in
            ( { model | curPage = newPage }, newMsg )

        ( NavbarMsg state, _ ) ->
            ( { model | navbarState = state }, Cmd.none )

        ( AboutMsg subMsg, AboutPage subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.About.update subMsg subModel
            in
            ( { model
                | curPage = AboutPage newSubModel
                , session = newSubModel.session
              }
            , Cmd.map AboutMsg newCmd
            )

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

        ( GotUserInfo userInfoResponse, _ ) ->
            case userInfoResponse of
                Err _ ->
                    ( { model | flow = Errored ErrHTTPGetUserInfo }
                    , Cmd.none
                    )

                Ok user ->
                    let
                        cred =
                            Maybe.withDefault
                                Credentials.default
                                model.session.cred

                        newCred =
                            { cred | user = Just user }

                        newSession =
                            Session.setUser model.session (LoggedIn user)
                    in
                    ( { model | flow = Done user, session = newSession }
                    , Credentials.store newCred
                    )

        ( Logout, _ ) ->
            let
                newSession =
                    Session.logout model.session

                ( newPage, newMsg ) =
                    changeRouteTo
                        (Just Route.Home)
                        newSession
                        model.searchParams

                newModel =
                    { model
                        | flow = Idle
                        , session = newSession
                        , curPage = newPage
                    }
            in
            ( newModel
            , Cmd.batch
                [ Credentials.storeCredentials Nothing
                , newMsg
                ]
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

        ( ProfileMsg subMsg, ProfilePage subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.Profile.update subMsg subModel
            in
            ( { model
                | curPage = ProfilePage newSubModel
                , session = newSubModel.session
              }
            , Cmd.map childTranslator newCmd
            )

        ( SetSearchParams params, _ ) ->
            let
                ( newPage, newMsg ) =
                    changeRouteTo (Just Route.Home) model.session (Just params)

                newModel =
                    { model
                        | curPage = newPage
                        , searchParams = Just params
                    }
            in
            ( newModel, newMsg )

        ( SignInMsg subMsg, SignInPage subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.SignIn.update subMsg subModel
            in
            ( { model
                | curPage = SignInPage newSubModel
                , session = newSubModel.session
              }
            , Cmd.map SignInMsg newCmd
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

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        navConfig =
            Navbar.config NavbarMsg
    in
    case model.curPage of
        AboutPage subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map AboutMsg (Page.About.view subModel))

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

        ProfilePage subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map childTranslator (Page.Profile.view subModel))

        SignInPage subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map SignInMsg (Page.SignIn.view subModel))

        SponsorsPage subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map SponsorsMsg (Page.Sponsors.view subModel))

        StudyPage nctId subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map StudyMsg (Page.Study.view subModel))


childTranslator : Page.Profile.Translator Msg
childTranslator =
    Page.Profile.translator
        { onInternalMessage = ProfileMsg
        , onSetSearchParams = SetSearchParams
        , onLogout = Logout
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.curPage of
        HomePage subModel ->
            Sub.map HomeMsg
                (Page.Home.subscriptions subModel)

        SignInPage subModel ->
            Sub.map SignInMsg
                (Page.SignIn.subscriptions subModel)

        --Redirect _ ->
        --    Sub.map GotCredentials
        --        (Credentials.onCredentialsChange
        --            (Decode.decodeString Credentials.decoder
        --                >> Result.withDefault Credentials.default
        --            )
        --        )
        _ ->
            Sub.none


changeRouteTo :
    Maybe Route
    -> Session
    -> Maybe SearchParams
    -> ( Page, Cmd Msg )
changeRouteTo maybeRoute session params =
    case maybeRoute of
        Just Route.About ->
            let
                ( subModel, subMsg ) =
                    Page.About.init session
            in
            ( AboutPage subModel, Cmd.map AboutMsg subMsg )

        Just Route.Cart ->
            let
                ( subModel, subMsg ) =
                    Page.Cart.init session
            in
            ( CartPage subModel, Cmd.map CartMsg subMsg )

        Just Route.Conditions ->
            let
                ( subModel, subMsg ) =
                    Page.Conditions.init session
            in
            ( ConditionsPage subModel, Cmd.map ConditionsMsg subMsg )

        Just Route.Home ->
            let
                ( subModel, subMsg ) =
                    Page.Home.init session params
            in
            ( HomePage subModel, Cmd.map HomeMsg subMsg )

        Just Route.Profile ->
            let
                ( subModel, subMsg ) =
                    Page.Profile.init session
            in
            ( ProfilePage subModel, Cmd.map childTranslator subMsg )

        Just Route.SignIn ->
            let
                --redirectUri =
                --    Url.fromString
                --        (Config.serverAddress ++ Route.routeToString Route.Home)
                ( subModel, subMsg ) =
                    Page.SignIn.init session
            in
            ( SignInPage subModel, Cmd.map SignInMsg subMsg )

        Just Route.Sponsors ->
            let
                ( subModel, subMsg ) =
                    Page.Sponsors.init session
            in
            ( SponsorsPage subModel, Cmd.map SponsorsMsg subMsg )

        Just (Route.Study nctId) ->
            let
                ( subModel, subMsg ) =
                    Page.Study.init session nctId
            in
            ( StudyPage nctId subModel, Cmd.map StudyMsg subMsg )

        _ ->
            let
                ( subModel, subMsg ) =
                    Page.Home.init session Nothing
            in
            ( HomePage subModel, Cmd.map HomeMsg subMsg )


getUserInfo : Page.SignIn.Configuration -> OAuth.Token -> Cmd Msg
getUserInfo configuration token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString configuration.userInfoEndpoint
        , expect = Http.expectJson GotUserInfo configuration.userInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
