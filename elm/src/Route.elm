module Route exposing (Route(..), fromUrl, href, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, int, oneOf, s, string)
import Url.Parser.Query as Query


type Route
    = Cart
    | Conditions
    | Home (Maybe String)
    | Study String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home (Parser.top <?> Query.string "condition")
        , Parser.map Conditions (s "conditions")
        , Parser.map Cart (s "cart")
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
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Cart ->
                    [ "cart" ]

                Conditions ->
                    [ "conditions" ]

                Home _ ->
                    []

                Study nctId ->
                    [ "study", nctId ]
    in
    "#/" ++ String.join "/" pieces
