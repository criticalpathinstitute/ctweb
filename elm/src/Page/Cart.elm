module Page.Cart exposing (ExternalMsg(..), Model, Msg(..), init, toSession, update, view)

-- import RemoteFile exposing (File)
-- import Page

import Cart exposing (Cart)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Icon
import RemoteData exposing (RemoteData(..))
import Session exposing (Session)


type alias Model =
    { session : Session

    -- , studies : RemoteData Http.Error (List Study)
    }


init :
    Session
    -> ( Model, Cmd Msg ) --Maybe Int -> ( Model, Cmd Msg )
init session =
    --id =
    let
        idList =
            Cart.toList (Session.getCart session)
    in
    ( { session = session

      -- , files = Loading
      }
    , Cmd.none
      --, Cmd.batch [ RemoteFile.fetchSome idList |> Http.send GetFilesCompleted ]
    )


toSession : Model -> Session
toSession model =
    model.session


type Msg
    = CartMsg Cart.Msg
    | EmptyCart



-- | GetFilesCompleted (Result Http.Error (List File))


type ExternalMsg
    = NoOp
    | SetCart Cart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- GetFilesCompleted result ->
        --     ( { model | files = RemoteData.fromResult result }, Cmd.none )
        CartMsg subMsg ->
            let
                newCart =
                    Cart.update subMsg (Session.getCart model.session)

                --newFiles =
                --    model.files
                --        |> RemoteData.toMaybe
                --        |> Maybe.withDefault []
                --        |> List.filter (\f -> Cart.contains newCart f.id)
                newSession =
                    Session.setCart model.session newCart
            in
            -- ( { model | session = newSession, files = Success newFiles }
            ( { model | session = newSession }
            , Cmd.batch
                [ --Cmd.map CartMsg subCmd
                  Cart.store newCart
                ]
            )

        EmptyCart ->
            let
                newSession =
                    Session.setCart model.session Cart.empty
            in
            -- ( { model | session = newSession, files = Success [] }
            ( { model | session = newSession }
            , Cart.store Cart.empty
            )


view : Model -> Html msg
view model =
    let
        cart =
            Session.getCart model.session
    in
    div []
        [ text <| "Cart has " ++ String.fromInt (Cart.size cart) ++ " items"
        ]



--view : Model -> Html Msg
--view model =
--    let
--        isLoggedIn =
--            False
--        --model.userId /= Nothing
--        cart =
--            Session.getCart model.session
--        count =
--            Cart.size cart
--        isEmpty =
--            count == 0
--    in
--    Page.viewRemoteData model.files
--        (\files ->
--            div [ class "container" ]
--                [ div [ class "pb-2 mt-5 mb-2" ]
--                    [ h1 [ class "font-weight-bold align-middle d-inline" ]
--                        [ span [ style "color" "dimgray" ] [ text "Cart" ]
--                        ]
--                    , span [ class "badge badge-pill badge-primary ml-2" ]
--                        [ if count == 0 then
--                            text ""
--                          else
--                            text (String.fromInt count)
--                        ]
--                    , button [ class "btn btn-primary mt-2 float-right", onClick EmptyCart, disabled isEmpty ]
--                        [ Icon.ban, text " Empty" ]
--                    ]
--                , viewCart cart files
--                ]
--        )
--viewCartControls :
--    Bool
--    -> Bool
--    -> Html Msg -- -> Maybe Int -> List SampleGroup -> Html Msg
--viewCartControls isEmpty isLoggedIn =
--    -- selectedCartId sampleGroups =
--    button [ class "btn btn-primary", onClick EmptyCart, disabled isEmpty ]
--        [ Icon.ban, text " Empty" ]
--viewCart : Cart -> List File -> Html Msg
--viewCart cart files =
--    if Cart.size cart == 0 then
--        div [ class "alert alert-secondary" ] [ text "The cart is empty" ]
--    else
--        Cart.view cart files Cart.Editable |> Html.map CartMsg
