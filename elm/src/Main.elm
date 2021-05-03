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
import Page.Cart
import Page.Conditions
import Page.Home
import Page.Searches
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
    = CartPage Page.Cart.Model
    | ConditionsPage Page.Conditions.Model
    | HomePage Page.Home.Model
      -- | Redirect Session
    | SignInPage Page.SignIn.Model
    | StudyPage String Page.Study.Model
    | SavedSearchesPage Page.Searches.Model
    | SponsorsPage Page.Sponsors.Model


type Msg
    = CartMsg Page.Cart.Msg
    | ConditionsMsg Page.Conditions.Msg
    | GotUserInfo (Result Http.Error User)
    | HomeMsg Page.Home.Msg
    | LinkClicked Browser.UrlRequest
    | NavbarMsg Navbar.State
    | SavedSearchesMsg Page.Searches.InternalMsg
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
            Nav.replaceUrl navKey (Url.toString redirectUrl)

        ( storedCart, bytes ) =
            case Decode.decodeValue flagsDecoder flags of
                Ok f ->
                    ( f.cart |> Maybe.withDefault CartData.empty, f.bytes )

                _ ->
                    ( CartData.empty, Nothing )

        ( newFlow, flowRedirectUri, cmd ) =
            case OAuth.parseToken url of
                OAuth.Empty ->
                    ( Idle, redirectUrl, clearUrl )

                OAuth.Success { token, state } ->
                    --let
                    --_ =
                    --    Debug.log "token" token
                    --_ =
                    --    Debug.log "state" state
                    --newSession =
                    --    Session.setState session (State (Url.toString stateUrl))
                    -- in
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

        _ =
            Debug.log "newFlow" newFlow

        --_ =
        --    Debug.log "flowRedirectUri" flowRedirectUri
        session =
            case Decode.decodeValue flagsDecoder flags of
                Ok f ->
                    let
                        cart =
                            f.cart |> Maybe.withDefault CartData.empty

                        ( cred, user ) =
                            case f.cred of
                                Just c ->
                                    ( Just c, Session.Guest )

                                _ ->
                                    ( Nothing, Session.Guest )
                    in
                    Session navKey State.default cart newFlow cred user

                Err error ->
                    Session navKey State.default CartData.empty newFlow Nothing Session.Guest

        model =
            { key = navKey
            , url = url
            , curPage = newPage
            , navbarState = navbarState
            , session = session
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
    let
        _ =
            Debug.log "model.flow" model.flow

        _ =
            Debug.log "msg" msg
    in
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

        ( SavedSearchesMsg subMsg, SavedSearchesPage subModel ) ->
            let
                ( newSubModel, newCmd ) =
                    Page.Searches.update subMsg subModel
            in
            ( { model
                | curPage = SavedSearchesPage newSubModel
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

        ( GotUserInfo userInfoResponse, _ ) ->
            case userInfoResponse of
                Err _ ->
                    ( { model | flow = Errored ErrHTTPGetUserInfo }
                    , Cmd.none
                    )

                Ok userInfo ->
                    let
                        newSession =
                            Session.setUser model.session (LoggedIn userInfo)
                    in
                    ( { model | flow = Done userInfo, session = newSession }
                    , Cmd.none
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

        SavedSearchesPage subModel ->
            PageView.view model.session
                navConfig
                model.navbarState
                (Html.map childTranslator (Page.Searches.view subModel))

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


childTranslator : Page.Searches.Translator Msg
childTranslator =
    Page.Searches.translator
        { onInternalMessage = SavedSearchesMsg
        , onSetSearchParams = SetSearchParams
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

        Just Route.SavedSearches ->
            let
                ( subModel, subMsg ) =
                    Page.Searches.init session
            in
            ( SavedSearchesPage subModel, Cmd.map childTranslator subMsg )

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
