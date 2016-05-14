module ByteBoard.Drawing exposing (Form(..), viewForms)

import Color
import Html exposing (Html)
import Svg exposing (Svg, svg, circle)
import Svg.Attributes as Attr


type alias Position =
    { x : Int, y : Int }


type Form
    = Blob Position


viewForms : ( Int, Int ) -> List Form -> Html msg
viewForms ( width, height ) forms =
    svg [ Attr.width => width, Attr.height => height ]
        (List.map viewForm forms)


(=>) : (String -> Svg.Attribute msg) -> a -> Svg.Attribute msg
(=>) attr value =
    attr (toString value)


viewForm : Form -> Svg msg
viewForm form =
    case form of
        Blob { x, y } ->
            circle
                [ Attr.r => 30
                , Attr.cx => x
                , Attr.cy => y
                , Attr.fill => Color.red
                ]
                []
