module PageView exposing (view)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (Document)
import Cart
import Html exposing (Html, a, div, img, input, span, text)
import Html.Attributes exposing (class, href, placeholder, src, style)
import Icon
import Route exposing (Route)
import Session exposing (Session)


view : Session -> Navbar.Config msg -> Navbar.State -> Html msg -> Document msg
view session navConfig navbarState content =
    let
        cartButton =
            let
                numItemsInCart =
                    Cart.size (Session.getCart session)

                label =
                    if numItemsInCart == 0 then
                        ""

                    else
                        String.fromInt numItemsInCart
            in
            a
                [ class "nav-link text-nowrap"
                , style "min-width" "4em"
                , Route.href Route.Cart
                ]
                [ Icon.shoppingCartLg
                , text " "
                , span [ class "gray absolute" ] [ text label ]
                ]

        nav =
            navConfig
                |> Navbar.withAnimation
                |> Navbar.brand
                    [ Route.href Route.Home ]
                    [ img
                        [ src "./assets/images/logo.png"
                        , class "d-inline-block align-top"
                        ]
                        []
                    ]
                |> Navbar.items
                    [ Navbar.itemLink [ Route.href Route.Cart ]
                        [ text "View Cart" ]
                    , Navbar.itemLink [] [ cartButton ]
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
