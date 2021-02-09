port module Cart exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html, a, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, href, target, type_)
import Html.Events exposing (onClick)
import Icon
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Route
import Set exposing (Set)



-- TYPES --


type Cart
    = Cart Model


type alias Model =
    { contents : Set Int -- Set of IDs for all files in cart
    , selected : Set Int -- Set of IDs for selected files in cart
    }


type CartType
    = Selectable
    | Editable



-- SERIALIZATION --


decoder : Decoder Cart
decoder =
    Decode.succeed Model
        |> required "contents"
            (Decode.list Decode.int |> Decode.map Set.fromList)
        |> optional "selected"
            (Decode.list Decode.int |> Decode.map Set.fromList)
            Set.empty
        |> Decode.map Cart


encode : Cart -> Value
encode (Cart cart) =
    Encode.object
        [ ( "contents", cart.contents |> Set.toList |> Encode.list Encode.int )
        ]


store : Cart -> Cmd msg
store cart =
    encode cart
        |> Just
        |> storeCart


port storeCart : Maybe Value -> Cmd msg


port onCartChange : (String -> msg) -> Sub msg



-- UTILITY FUNCTIONS --


empty : Cart
empty =
    Cart (Model Set.empty Set.empty)


size : Cart -> Int
size (Cart cart) =
    Set.size cart.contents


toList : Cart -> List Int
toList (Cart cart) =
    cart.contents |> Set.toList


contains : Cart -> Int -> Bool
contains (Cart cart) id =
    Set.member id cart.contents


add : Cart -> List Int -> Cart
add (Cart cart) ids =
    Cart { cart | contents = Set.union (Set.fromList ids) cart.contents }


remove : Cart -> List Int -> Cart
remove (Cart cart) ids =
    Cart { cart | contents = Set.diff cart.contents (Set.fromList ids) }


selected : Cart -> Int -> Bool
selected (Cart cart) id =
    Set.member id cart.selected


selectedToList : Cart -> List Int
selectedToList (Cart cart) =
    cart.selected |> Set.toList


select : Cart -> Int -> Cart
select (Cart cart) id =
    Cart { cart | selected = Set.insert id cart.selected }


selectAll : Cart -> Cart
selectAll (Cart cart) =
    Cart { cart | selected = cart.contents }


unselect : Cart -> Int -> Cart
unselect (Cart cart) id =
    Cart { cart | selected = Set.remove id cart.selected }


unselectAll : Cart -> Cart
unselectAll (Cart cart) =
    Cart { cart | selected = Set.empty }



-- UPDATE --


type Msg
    = AddToCart (List Int)
    | RemoveFromCart (List Int)
    | ToggleSelectInCart Int
    | SelectAllInCart
    | UnselectAllInCart


update : Msg -> Cart -> Cart
update msg cart =
    case msg of
        AddToCart idList ->
            add cart idList

        RemoveFromCart idList ->
            remove cart idList

        ToggleSelectInCart id ->
            if selected cart id then
                unselect cart id

            else
                select cart id

        SelectAllInCart ->
            selectAll cart

        UnselectAllInCart ->
            unselectAll cart



-- VIEW --


view :
    Cart
    ->
        List
            { a
                | id : Int
                , url : String
                , sampleId : Int
                , sampleAccn : String
                , experimentId : Int
                , experimentAccn : String
                , source : String
                , layout : String
                , runAccn : String
                , projectId : Int
                , projectName : String
            }
    -> CartType
    -> Html Msg
view cart files cartType =
    text ""


addToCartButton : Cart -> Int -> Html Msg
addToCartButton cart id =
    let
        btn label clickMsg =
            Button.button
                [ Button.outlinePrimary
                , Button.onClick clickMsg
                , Button.attrs [ Spacing.mx1 ]
                ]
                [ text label ]
    in
    if contains cart id then
        btn "Remove" (RemoveFromCart [ id ])

    else
        btn "Add" (AddToCart [ id ])


addAllToCartButton : Cart -> List Int -> Html Msg
addAllToCartButton cart ids =
    Button.button
        [ Button.outlinePrimary
        , Button.onClick (AddToCart ids)
        , Button.attrs [ Spacing.mx1 ]
        ]
        [ Icon.shoppingCart
        , text " "
        , text "Add All"
        ]
