module ByteBoard.Types exposing (Position, Size, delta, hypotenuse)


type alias Position =
    { x : Int, y : Int }


type alias Size =
    { width : Int, height : Int }


delta : Position -> Position -> Position
delta a b =
    { x = a.x - b.x, y = a.y - b.y }


hypotenuse : Position -> Int
hypotenuse pos =
    round <| sqrt <| toFloat (pos.x * pos.x + pos.y * pos.y)
