module ByteBoard.Drawing exposing (Form(..), viewForms)

import Color exposing (Color)
import Color.Convert exposing (colorToCssRgba)
import Html exposing (Html, div)
import Svg exposing (Svg, svg, circle)
import Svg.Attributes as Attr


type alias Position =
    { x : Int, y : Int }


type Form
    = Blob Position


gridBg : String
gridBg =
    """
        background-position: -1px -1px;
        background-image:
            linear-gradient(to right, #ccc 1px, transparent 1px),
            linear-gradient(to bottom, #ccc 1px, transparent 1px);
        background-size: 25px 25px
    """


viewForms : ( Int, Int ) -> List Form -> Html msg
viewForms ( width, height ) forms =
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
