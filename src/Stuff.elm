module Stuff exposing (..)

import Html exposing (Html, text)


tuplemap2 : (a -> b) -> ( a, a ) -> ( b, b )
tuplemap2 f ( x, y ) =
    ( f x, f y )


push : List a -> a -> List a
push l x =
    l ++ [ x ]


(??) : Bool -> Html msg -> Html msg
(??) condition body =
    if condition then
        body
    else
        text ""


(=>) : a -> b -> ( a, b )
(=>) k v =
    ( k, v )
