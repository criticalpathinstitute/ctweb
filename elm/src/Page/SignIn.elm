port module Page.SignIn exposing (..)

import Base64.Encode as Base64
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Utilities.Spacing as Spacing
import Browser.Navigation as Navigation
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Cart exposing (Cart)
import Common exposing (commify, viewHttpErrorMessage)
import Config exposing (signInRedirectFragment, signInRedirectHost)
import Html exposing (Attribute, Html, a, button, div, h1, h2, img, li, p, span, text, ul)
import Html.Attributes exposing (class, href, src, style, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import OAuth
import OAuth.Implicit as OAuth
import RemoteData exposing (RemoteData, WebData)
import Route exposing (Route)
import Session exposing (Session)
import Types exposing (Flow(..), FlowError(..))
import Url exposing (Protocol(..), Url)
import Url.Builder
import User exposing (User)


type alias Model =
    { session : Session
    , redirectUri : Url
    }


type alias Configuration =
    { authorizationEndpoint : Url
    , userInfoEndpoint : Url
    , userInfoDecoder : Decoder User
    , clientId : String
    , scope : List String
    }


type alias State =
    { state : String }


type Msg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | GotAccessToken (Result Http.Error OAuth.AuthorizationSuccess)
    | UserInfoRequested
    | GotUserInfo (Result Http.Error User)
    | SignOutRequested


init : Session -> ( Model, Cmd Msg )
init session =
    let
        redirectUri =
            { defaultHttpUrl
                | host = signInRedirectHost
                , path = signInRedirectFragment
            }

        _ =
            Debug.log "redirectUri" (Url.toString redirectUri)
    in
    ( { session = session, redirectUri = redirectUri }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.session.flow, msg ) of
        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        --( Authorized token, UserInfoRequested ) ->
        --    userInfoRequested model token
        ( Authorized _, GotUserInfo userInfoResponse ) ->
            let
                _ =
                    Debug.log "userInfoResponse" userInfoResponse
            in
            gotUserInfo model userInfoResponse

        --( Done _, SignOutRequested ) ->
        --    signOutRequested model
        _ ->
            ( model, Cmd.none )


subscriptions =
    always <| randomBytes GotRandomBytes


getUserInfo : OAuth.Token -> Cmd Msg
getUserInfo token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString configuration.userInfoEndpoint
        , expect = Http.expectJson GotUserInfo configuration.userInfoDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| OAuth configuration.

Note that this demo also fetches basic user information
with the obtained access token,
hence the user info endpoint and JSON decoder

-}
configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl
            | host = "criticalpathinst.us.auth0.com"
            , path = "/authorize"
        }
    , userInfoEndpoint =
        { defaultHttpsUrl
            | host = "criticalpathinst.us.auth0.com"
            , path = "/userinfo"
        }
    , userInfoDecoder = User.userDecoder
    , clientId = "WtqPTRMEOWroj4izPpM6cTbUBES1GtHI"
    , scope =
        [ "profile", "openid", "email" ]
    }


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


defaultHttpUrl : Url
defaultHttpUrl =
    { protocol = Http
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( model
    , genRandomBytes 16
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    let
        state =
            convertBytes bytes

        authorization =
            { clientId = configuration.clientId
            , redirectUri = model.redirectUri
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    ( model
    , authorization
        |> OAuth.makeAuthorizationUrl
        |> Url.toString
        |> Navigation.load
    )


gotUserInfo : Model -> Result Http.Error User -> ( Model, Cmd Msg )
gotUserInfo model userInfoResponse =
    case userInfoResponse of
        Err _ ->
            -- ( { model | flow = Errored ErrHTTPGetUserInfo }
            ( model
            , Cmd.none
            )

        Ok userInfo ->
            -- ( { model | flow = Done userInfo }
            ( model
            , Cmd.none
            )



--signOutRequested : Model -> ( Model, Cmd Msg )
--signOutRequested model =
--    -- ( { model | flow = Idle }
--    ( model
--    , Navigation.load (Url.toString model.redirectUri)
--    )


toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


convertBytes : List Int -> String
convertBytes =
    toBytes >> Base64.bytes >> Base64.encode


decoder : Decoder State
decoder =
    Json.Decode.succeed State
        |> optional "bytes"
            (Json.Decode.list Json.Decode.int |> Json.Decode.map convertBytes)
            ""



--
-- View
--


type alias ViewConfiguration msg =
    { title : String
    , btnClass : Attribute msg
    }



--view : Model -> Html Msg
--view model =
--    Grid.container []
--        [ Grid.row []
--            [ Grid.col [] [ text "Sign In" ]
--            ]
--        ]
--view : ViewConfiguration Msg -> Model -> Document Msg
--view ({ title } as config) model =
--    { title = title
--    , body = viewBody config model
--    }


view : Model -> Html Msg
view model =
    let
        config =
            { title = "Auth0"
            , btnClass = class "btn-auth0"
            }
    in
    --viewBody config model
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ Button.button
                    [ Button.onClick SignInRequested
                    , Button.primary
                    ]
                    [ text "Sign In Using Auth0" ]
                ]
            ]
        ]


viewBody : ViewConfiguration Msg -> Model -> Html Msg
viewBody config model =
    div [ class "flex", class "flex-column", class "flex-space-around" ] <|
        case model.session.flow of
            Idle ->
                div [ class "flex" ]
                    [ viewAuthorizationStep False
                    , viewStepSeparator False
                    , viewGetUserInfoStep False
                    ]
                    :: viewIdle config

            Authorized _ ->
                div [ class "flex" ]
                    [ viewAuthorizationStep True
                    , viewStepSeparator True
                    , viewGetUserInfoStep False
                    ]
                    :: viewAuthorized

            Done userInfo ->
                div [ class "flex" ]
                    [ viewAuthorizationStep True
                    , viewStepSeparator True
                    , viewGetUserInfoStep True
                    ]
                    :: viewUserInfo config userInfo

            Errored err ->
                div [ class "flex" ]
                    [ viewErroredStep
                    ]
                    :: viewErrored err


viewIdle : ViewConfiguration Msg -> List (Html Msg)
viewIdle { btnClass } =
    [ button
        [ onClick SignInRequested, btnClass ]
        [ text "Sign in" ]
    ]


viewAuthorized : List (Html Msg)
viewAuthorized =
    [ span [] [ text "Getting user info..." ]
    ]


viewUserInfo : ViewConfiguration Msg -> User -> List (Html Msg)
viewUserInfo { btnClass } { name, picture } =
    [ div [ class "flex", class "flex-column" ]
        [ img [ class "avatar", src picture ] []
        , p [] [ text name ]
        , div []
            [ button
                [ onClick SignOutRequested, btnClass ]
                [ text "Sign out" ]
            ]
        ]
    ]


viewErrored : FlowError -> List (Html Msg)
viewErrored error =
    [ span [ class "span-error" ] [ viewError error ] ]


viewError : FlowError -> Html Msg
viewError e =
    text <|
        case e of
            ErrStateMismatch ->
                "'state' doesn't match, the request has likely been forged by an adversary!"

            ErrAuthorization error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrHTTPGetUserInfo ->
                "Unable to retrieve user info: HTTP request failed."


viewAuthorizationStep : Bool -> Html Msg
viewAuthorizationStep isActive =
    viewStep isActive ( "Authorization", style "left" "-110%" )


viewGetUserInfoStep : Bool -> Html Msg
viewGetUserInfoStep isActive =
    viewStep isActive ( "Get User Info", style "left" "-135%" )


viewErroredStep : Html Msg
viewErroredStep =
    div
        [ class "step", class "step-errored" ]
        [ span [ style "left" "-50%" ] [ text "Errored" ] ]


viewStep : Bool -> ( String, Attribute Msg ) -> Html Msg
viewStep isActive ( step, position ) =
    let
        stepClass =
            class "step"
                :: (if isActive then
                        [ class "step-active" ]

                    else
                        []
                   )
    in
    div stepClass [ span [ position ] [ text step ] ]


viewStepSeparator : Bool -> Html Msg
viewStepSeparator isActive =
    let
        stepClass =
            class "step-separator"
                :: (if isActive then
                        [ class "step-active" ]

                    else
                        []
                   )
    in
    span stepClass []


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


port genRandomBytes : Int -> Cmd msg


port randomBytes : (List Int -> msg) -> Sub msg
