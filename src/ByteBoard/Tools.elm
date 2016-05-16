module ByteBoard.Tools exposing (Tool(..), init, view)

import Html exposing (Html, ul, li, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Stuff exposing ((=>))


type Tool
    = Select
    | Blob
    | Line


init : Tool
init =
    Select


listStyle : Html.Attribute a
listStyle =
    Attr.style
        [ "list-style-type" => "none"
        , "margin" => "0"
        , "padding" => "0"
        ]


view : Tool -> Html Tool
view tool =
    ul [ listStyle ]
        [ viewTool tool Select
        , viewTool tool Blob
        , viewTool tool Line
        ]


itemStyle : Bool -> Html.Attribute a
itemStyle selected =
    Attr.style
        ([ "padding" => "5px"
         , "margin" => "5px 0"
         , "border" => "1px solid #333"
         , "cursor" => "pointer"
         ]
            ++ if selected then
                [ "background" => "#ccc" ]
               else
                []
        )


viewTool : Tool -> Tool -> Html Tool
viewTool current tool =
    li [ itemStyle (current == tool), onClick tool ]
        [ text (toString tool) ]
