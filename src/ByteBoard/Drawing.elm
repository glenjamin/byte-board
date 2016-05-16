module ByteBoard.Drawing exposing (Model, Msg(..), init, update, view)

import String
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgba)
import Html exposing (Html, div)
import Svg exposing (Svg, svg, text, path, circle)
import Svg.Attributes as Attr
import Stuff exposing (push, maybePush)
import ByteBoard.Types exposing (Size, Position, delta)
import ByteBoard.Tools as Tools


type alias Model =
    { pending : List Position
    , drawings : List Form
    }


type Form
    = Blob Position
    | Line Position Position


init : Tools.Tool -> Model
init tool =
    { pending = []
    , drawings = [ Blob { x = 100, y = 100 } ]
    }


type Msg
    = Click Position



-- TODO: have own messages, (Click tool pos) via `view`


update : Msg -> Tools.Tool -> Model -> Model
update msg tool model =
    case msg of
        Click pos ->
            if tool == Tools.Select then
                model
            else
                case draw model tool pos of
                    Nothing ->
                        { model | pending = push model.pending pos }

                    Just form ->
                        { model
                            | pending = []
                            , drawings = push model.drawings form
                        }


draw : Model -> Tools.Tool -> Position -> Maybe Form
draw { pending } tool pos =
    case tool of
        Tools.Select ->
            Nothing

        Tools.Blob ->
            Just (Blob pos)

        Tools.Line ->
            List.head pending
                |> Maybe.map (\x -> Line x (delta pos x))


canvasStyle : String
canvasStyle =
    """
        cursor: crosshair;
        background-position: -1px -1px;
        background-image:
            linear-gradient(to right, #ccc 1px, transparent 1px),
            linear-gradient(to bottom, #ccc 1px, transparent 1px);
        background-size: 25px 25px
    """


view : Size -> Tools.Tool -> Position -> Model -> Html msg
view { width, height } tool mouse model =
    div [ Attr.style canvasStyle ]
        [ svg [ Attr.width =+ width, Attr.height =+ height ]
            (maybePush (List.map viewForm model.drawings)
                (Maybe.map viewForm (draw model tool mouse))
            )
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

        Line start delta ->
            path
                [ makePath [ MoveTo start.x start.y, PathTo delta.x delta.y ]
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
