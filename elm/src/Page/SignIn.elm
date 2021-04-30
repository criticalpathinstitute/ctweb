port module Page.SignIn exposing (Model, Msg, init, subscriptions, update, view)

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
import Config exposing (apiServer, maxCartSize, serverAddress)
import Html exposing (Attribute, Html, a, button, div, h1, h2, img, li, p, span, text, ul)
import Html.Attributes exposing (class, href, src, style, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import OAuth
import OAuth.Implicit as OAuth
import RemoteData exposing (RemoteData, WebData)
import Route
import Session exposing (Session)
import Types exposing (UserInfo)
import Url exposing (Protocol(..), Url)
import Url.Builder


type alias Model =
    { session : Session
    , flow : Flow
    , redirectUrl : Maybe Url
    }


type alias Configuration =
    { authorizationEndpoint : Url
    , userInfoEndpoint : Url
    , userInfoDecoder : Decoder UserInfo
    , clientId : String
    , scope : List String
    }


type Msg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | GotAccessToken (Result Http.Error OAuth.AuthorizationSuccess)
    | UserInfoRequested
    | GotUserInfo (Result Http.Error UserInfo)
    | SignOutRequested


type Flow
    = Idle
    | Authorized OAuth.Token
    | Done UserInfo
    | Errored Error


type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrHTTPGetUserInfo


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, flow = Idle, redirectUrl = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.flow, msg ) of
        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Authorized token, UserInfoRequested ) ->
            userInfoRequested model token

        ( Authorized _, GotUserInfo userInfoResponse ) ->
            gotUserInfo model userInfoResponse

        ( Done _, SignOutRequested ) ->
            signOutRequested model

        _ ->
            ( model, Cmd.none )



--subscriptions : Model -> Sub Msg
--subscriptions model =
-- subscriptions : Sub Msg


subscriptions =
    always <| randomBytes GotRandomBytes


getUserInfo : Configuration -> OAuth.Token -> Cmd Msg
getUserInfo { userInfoDecoder, userInfoEndpoint } token =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , headers = OAuth.useToken token []
        , url = Url.toString userInfoEndpoint
        , expect = Http.expectJson GotUserInfo userInfoDecoder
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
            | host = "accounts.google.com"
            , path = "/o/oauth2/v2/auth"
        }
    , userInfoEndpoint =
        { defaultHttpsUrl
            | host = "www.googleapis.com"
            , path = "/oauth2/v1/userinfo"
        }
    , userInfoDecoder =
        Json.Decode.map2 UserInfo
            (Json.Decode.field "name" Json.Decode.string)
            (Json.Decode.field "picture" Json.Decode.string)
    , clientId = "" -- Get this
    , scope =
        [ "profile" ]
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


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( { model | flow = Idle }
    , genRandomBytes 16
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    case model.redirectUrl of
        Just url ->
            let
                { state } =
                    convertBytes bytes

                authorization =
                    { clientId = configuration.clientId
                    , redirectUri = url
                    , scope = configuration.scope
                    , state = Just state
                    , url = configuration.authorizationEndpoint
                    }
            in
            ( { model | flow = Idle }
            , authorization
                |> OAuth.makeAuthorizationUrl
                |> Url.toString
                |> Navigation.load
            )

        _ ->
            ( model, Cmd.none )


userInfoRequested : Model -> OAuth.Token -> ( Model, Cmd Msg )
userInfoRequested model token =
    ( { model | flow = Authorized token }
    , getUserInfo configuration token
    )


gotUserInfo : Model -> Result Http.Error UserInfo -> ( Model, Cmd Msg )
gotUserInfo model userInfoResponse =
    case userInfoResponse of
        Err _ ->
            ( { model | flow = Errored ErrHTTPGetUserInfo }
            , Cmd.none
            )

        Ok userInfo ->
            ( { model | flow = Done userInfo }
            , Cmd.none
            )


signOutRequested : Model -> ( Model, Cmd Msg )
signOutRequested model =
    case model.redirectUrl of
        Just url ->
            ( { model | flow = Idle }, Navigation.load (Url.toString url) )

        _ ->
            ( model, Cmd.none )


toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


convertBytes : List Int -> { state : String }
convertBytes =
    toBytes >> base64 >> (\state -> { state = state })



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
            { title = "Google - Flow: Implicit"
            , btnClass = class "btn-google"
            }
    in
    div [] (viewBody config model)


viewBody : ViewConfiguration Msg -> Model -> List (Html Msg)
viewBody config model =
    [ div [ class "flex", class "flex-column", class "flex-space-around" ] <|
        case model.flow of
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
    ]


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


viewUserInfo : ViewConfiguration Msg -> UserInfo -> List (Html Msg)
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


viewErrored : Error -> List (Html Msg)
viewErrored error =
    [ span [ class "span-error" ] [ viewError error ] ]


viewError : Error -> Html Msg
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
