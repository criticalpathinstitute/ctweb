module PageView exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document)
import Cart
import Html exposing (Html, a, div, img, input, span, text)
import Html.Attributes exposing (class, href, placeholder, src, style, target)
import Icon
import Route exposing (Route)
import Session exposing (Session)


view : Session -> Navbar.Config msg -> Navbar.State -> Html msg -> Document msg
view session navConfig navbarState content =
    let
        cartButton =
            let
                cartSize =
                    Cart.size (Session.getCart session)

                label =
                    if cartSize == 0 then
                        ""

                    else
                        String.fromInt cartSize
            in
            div []
                [ Icon.shoppingCartLg
                , text " "
                , span [ class "gray absolute" ] [ text label ]
                ]

        nav =
            navConfig
                |> Navbar.withAnimation
                |> Navbar.brand
                    [ Route.href (Route.Home Nothing) ]
                    [ img
                        [ src "./assets/images/logo.png"
                        , class "d-inline-block align-top"
                        ]
                        []
                    ]
                |> Navbar.items
                    [ Navbar.itemLink
                        [ Route.href Route.Conditions
                        , target "_blank"
                        ]
                        [ text "Conditions" ]
                    , Navbar.itemLink
                        [ Route.href Route.Sponsors
                        , target "_blank"
                        ]
                        [ text "Sponsors" ]
                    , Navbar.itemLink
                        [ Route.href Route.SavedSearches
                        , target "_blank"
                        ]
                        [ text "Searches" ]
                    , Navbar.itemLink
                        [ Route.href Route.Cart
                        , target "_blank"
                        ]
                        [ cartButton ]
                    ]
                |> Navbar.view navbarState
    in
    { title = "Clinical Trials Portal"
    , body =
        [ Grid.container []
            [ nav
            , content
            ]
        ]
    }
