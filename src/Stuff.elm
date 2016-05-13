module Stuff exposing (..)


tuplemap2 : (a -> b) -> ( a, a ) -> ( b, b )
tuplemap2 f ( x, y ) =
  ( f x, f y )


push : List a -> a -> List a
push l x =
  l ++ [ x ]


(=>) : a -> b -> ( a, b )
(=>) k v =
  ( k, v )
