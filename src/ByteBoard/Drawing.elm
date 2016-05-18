module ByteBoard.Drawing
    exposing
        ( Model
        , Msg(..)
        , init
        , update
        , view
        , mousePos
        )

import String
import Json.Decode as Json exposing ((:=))
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgba)
import Html exposing (Html, div)
import Html.Events exposing (onClick, on)
import Svg exposing (Svg, svg, text, path, circle)
import Svg.Attributes as Attr
import Stuff exposing (push, maybePush)
import ByteBoard.Types exposing (Size, Position, delta)
import ByteBoard.Tools as Tools


type Model
    = Model ModelData


type alias ModelData =
    { mouse : Position
    , pending : List Position
    , drawings : List Form
    }


type Form
    = Blob Position
    | Line Position Position


mousePos : Model -> Position
mousePos (Model { mouse }) =
    mouse


init : Model
init =
    Model
        { mouse = { x = 0, y = 0 }
        , pending = []
        , drawings = [ Blob { x = 100, y = 100 } ]
        }


type Msg
    = Abort
    | Mouse Position
    | Click Tools.Tool


update : Msg -> Model -> Model
update msg ((Model data) as model) =
    case msg of
        Abort ->
            Model { data | pending = [] }

        Mouse pos ->
            Model { data | mouse = pos }

        Click tool ->
            if tool == Tools.Select then
                model
            else
                case draw data tool of
                    Nothing ->
                        Model { data | pending = push data.pending data.mouse }

                    Just form ->
                        Model
                            { data
                                | pending = []
                                , drawings = push data.drawings form
                            }


draw : ModelData -> Tools.Tool -> Maybe Form
draw { pending, mouse } tool =
    case tool of
        Tools.Select ->
            Nothing

        Tools.Blob ->
            Just (Blob mouse)

        Tools.Line ->
            List.head pending
                |> Maybe.map (\x -> Line x (delta mouse x))


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


view : Size -> Tools.Tool -> Model -> Html Msg
view { width, height } tool (Model model) =
    div
        [ Attr.style canvasStyle
        , onClick (Click tool)
        , onMouseMove Mouse
        ]
        [ svg [ Attr.width =+ width, Attr.height =+ height ]
            (maybePush (List.map viewForm model.drawings)
                (Maybe.map viewForm (draw model tool))
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


onMouseMove : (Position -> Msg) -> Html.Attribute Msg
onMouseMove msg =
    on "mousemove" (Json.map msg relativeMousePosition)


relativeMousePosition : Json.Decoder Position
relativeMousePosition =
    Json.object2 Position ("offsetX" := Json.int) ("offsetY" := Json.int)
