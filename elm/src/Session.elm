module Session exposing (..)

import Browser.Navigation as Nav
import Cart exposing (Cart)
import Credentials exposing (Credentials)
import State exposing (State)
import Types exposing (Flow(..))
import User exposing (User)


type SessionUser
    = LoggedIn User
    | Guest


type alias Session =
    { navKey : Nav.Key
    , state : State
    , cart : Cart
    , flow : Flow
    , cred : Maybe Credentials
    , user : SessionUser
    }


setCart : Session -> Cart -> Session
setCart session cart =
    { session | cart = cart }


setUser : Session -> SessionUser -> Session
setUser session user =
    { session | user = user }



--type Session
--    = LoggedIn Nav.Key State Cart Flow Credentials
--    | Guest Nav.Key State Cart Flow
--navKey : Session -> Nav.Key
--navKey session =
--    case session of
--        LoggedIn key _ _ _ _ ->
--            key
--        Guest key _ _ _ ->
--            key
--getState : Session -> State
--getState session =
--    case session of
--        LoggedIn _ state _ _ _ ->
--            state
--        Guest _ state _ _ ->
--            state
--setState : Session -> State -> Session
--setState session state =
--    case session of
--        LoggedIn key _ cart flow cred ->
--            LoggedIn key state cart flow cred
--        Guest key _ cart flow ->
--            Guest key state cart flow
--getCart : Session -> Cart
--getCart session =
--    case session of
--        LoggedIn _ _ c _ _ ->
--            c
--        Guest _ _ c _ ->
--            c
--setCart : Session -> Cart -> Session
--setCart session cart =
--    case session of
--        LoggedIn key state _ flow cred ->
--            LoggedIn key state cart flow cred
--        Guest key state _ flow ->
--            Guest key state cart flow
--credentials : Session -> Maybe Credentials
--credentials session =
--    case session of
--        LoggedIn _ _ _ _ cred ->
--            Just cred
--        Guest _ _ _ _ ->
--            Nothing
--setCredentials : Session -> Credentials -> Session
--setCredentials session cred =
--    case session of
--        LoggedIn key state cart flow _ ->
--            LoggedIn key state cart flow cred
--        Guest key state cart flow ->
--            LoggedIn key state cart flow cred
--token : Session -> String
--token session =
--    case session of
--        LoggedIn _ _ _ _ cred ->
--            cred.token
--        Guest _ _ _ _ ->
--            ""
--getFlow : Session -> Flow
--getFlow session =
--    case session of
--        LoggedIn _ _ _ flow _ ->
--            flow
--        Guest _ _ _ flow ->
--            flow
--getUser : Session -> Maybe User
--getUser session =
--    case session of
--        LoggedIn _ _ _ _ cred ->
--            cred.user
--        Guest _ _ _ _ ->
--            Nothing
--setUser : Session -> User -> Session
--setUser session user =
--    let
--        default =
--            Credentials.default
--    in
--    case session of
--        LoggedIn key state cart flow cred ->
--            LoggedIn key state cart flow { cred | user = Just user }
--        Guest key state cart flow ->
--            LoggedIn key state cart flow { default | user = Just user }
--logout : Session -> Session
--logout session =
--    case session of
--        LoggedIn key state cart flow _ ->
--            Guest key state cart flow
--        _ ->
--            session
