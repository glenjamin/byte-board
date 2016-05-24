module AbstractFormAttempt exposing (main)

import Html.App as Html
import Html
import ByteBoard.Types exposing (Position, Bounds, delta, hypotenuse, bounds)


type alias Drawing data =
    { create : List Position -> Maybe data
    , render : data -> String
    }


twoPoints : (Position -> Position -> data) -> List Position -> Maybe data
twoPoints f points =
    case List.take 2 points of
        [ a, b ] ->
            Just (f a b)

        _ ->
            Nothing


circle : Drawing { centre : Position, radius : Int }
circle =
    { create =
        twoPoints (\a b -> { centre = a, radius = hypotenuse (delta b a) })
    , render =
        (\{ centre, radius } ->
            "Circle at " ++ (toString centre) ++ " r=" ++ (toString radius)
        )
    }


box : Drawing { bounds : Bounds }
box =
    { create =
        twoPoints (\a b -> { bounds = bounds a b })
    , render =
        (\{ bounds } -> "Box from " ++ (toString bounds))
    }


line : Drawing { from : Position, to : Position }
line =
    { create =
        twoPoints (\a b -> { from = a, to = b })
    , render =
        (\{ from, to } ->
            "Line from " ++ (toString from) ++ " to " ++ (toString to)
        )
    }


data =
    List.filterMap identity
        [ circle.create [ { x = 0, y = 0 }, { x = 10, y = 0 } ]
        , box.create [ { x = 0, y = 0 }, { x = 10, y = 0 } ]
        , line.create [ { x = 0, y = 0 }, { x = 10, y = 0 } ]
        ]


result =
    []


main : Program Never
main =
    Html.beginnerProgram
        { model = ( Debug.log "data" data, Debug.log "result" result )
        , view = (\x -> Html.text "")
        , update = (\x y -> y)
        }
