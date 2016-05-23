module ByteBoard.Types
    exposing
        ( Position
        , Size
        , Bounds
        , delta
        , hypotenuse
        , bounds
        )


type alias Position =
    { x : Int, y : Int }


type alias Size =
    { width : Int, height : Int }


type alias Bounds =
    { x : Int, y : Int, dx : Int, dy : Int }


delta : Position -> Position -> Position
delta a b =
    { x = a.x - b.x, y = a.y - b.y }


hypotenuse : Position -> Int
hypotenuse pos =
    round <| sqrt <| toFloat (pos.x * pos.x + pos.y * pos.y)


bounds : Position -> Position -> Bounds
bounds a b =
    let
        d =
            delta b a
    in
        normalize { x = a.x, y = a.y, dx = d.x, dy = d.y }


normalize : Bounds -> Bounds
normalize { x, y, dx, dy } =
    let
        ( x', dx' ) =
            if dx < 0 then
                ( x + dx, abs dx )
            else
                ( x, dx )

        ( y', dy' ) =
            if dy < 0 then
                ( y + dy, abs dy )
            else
                ( y, dy )
    in
        { x = x', y = y', dx = dx', dy = dy' }
