module Route exposing (Route(..), fromUrl, href, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Types exposing (SearchParams)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, int, oneOf, s, string)
import Url.Parser.Query as Query


type Route
    = Cart
    | Conditions
    | Home
    | SignIn
    | SavedSearches
    | Sponsors
    | Study String



-- [ Parser.map Home (s "search" <?> Query.string "condition")


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Home (s "search")
        , Parser.map Conditions (s "conditions")
        , Parser.map SavedSearches (s "searches")
        , Parser.map Sponsors (s "sponsors")
        , Parser.map Cart (s "cart")
        , Parser.map SignIn (s "signin")
        , Parser.map Study (s "study" </> string)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    --Parser.parse routeParser url
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse routeParser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pathParts =
            case page of
                Cart ->
                    [ "cart" ]

                Conditions ->
                    [ "conditions" ]

                Home ->
                    [ "search" ]

                --Home (Just params) ->
                --    =" ++ condition ], [ "search" ] )
                Study nctId ->
                    [ "study", nctId ]

                SavedSearches ->
                    [ "searches" ]

                SignIn ->
                    [ "signin" ]

                Sponsors ->
                    [ "sponsors" ]
    in
    "#/" ++ String.join "/" pathParts
