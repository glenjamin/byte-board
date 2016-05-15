module ByteBoard.Drawing exposing (Form, init, draw, view)

import String
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgba)
import Html exposing (Html, div)
import Svg exposing (Svg, svg, path, circle)
import Svg.Attributes as Attr
import ByteBoard.Types exposing (Size, Position)
import ByteBoard.Tools as Tools


type Form
    = Blob Position
    | Line Position


init : List Form
init =
    [ Blob { x = 100, y = 100 } ]


draw : Tools.Tool -> Position -> Form
draw tool pos =
    case tool of
        Tools.Blob ->
            Blob pos

        Tools.Line ->
            Line pos


gridBg : String
gridBg =
    """
        background-position: -1px -1px;
        background-image:
            linear-gradient(to right, #ccc 1px, transparent 1px),
            linear-gradient(to bottom, #ccc 1px, transparent 1px);
        background-size: 25px 25px
    """


view : Size -> List Form -> Html msg
view { width, height } forms =
    div [ Attr.style gridBg ]
        [ svg [ Attr.width =+ width, Attr.height =+ height ]
            (List.map viewForm forms)
        ]


(=+) : (String -> Svg.Attribute msg) -> a -> Svg.Attribute msg
(=+) attr value =
    attr (toString value)


(🖌) : (String -> Svg.Attribute msg) -> Color -> Svg.Attribute msg
(🖌) attr value =
    attr (colorToCssRgba value)


redDot : Svg msg
redDot =
    circle [ Attr.fill 🖌 Color.red ] []


coloured : (String -> Svg.Attribute msg) -> Color -> Svg.Attribute msg
coloured attr value =
    attr (colorToCssRgba value)


viewForm : Form -> Svg msg
viewForm form =
    case form of
        Blob { x, y } ->
            circle
                [ Attr.r =+ 30
                , Attr.cx =+ x
                , Attr.cy =+ y
                , Attr.fill 🖌 Color.red
                ]
                []

        Line { x, y } ->
            path
                [ makePath [ (MoveTo 0 0), (PathTo x y) ]
                , Attr.stroke 🖌 Color.blue
                , Attr.strokeWidth =+ 5
                ]
                []


type Path
    = MoveTo Int Int
    | PathTo Int Int


makePath : List Path -> Svg.Attribute a
makePath paths =
    paths
        |> List.concatMap
            (\p ->
                case p of
                    MoveTo x y ->
                        [ "m", toString x, toString y ]

                    PathTo x y ->
                        [ "l", toString x, toString y ]
            )
        |> String.join " "
        |> Attr.d
