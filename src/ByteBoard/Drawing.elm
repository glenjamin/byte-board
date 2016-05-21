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
import Html.Events as Events
import Svg exposing (Svg, svg, text, path, circle)
import Svg.Attributes as Attr
import Stuff exposing (push, maybePush)
import ByteBoard.Types exposing (Size, Position, delta, hypotenuse)
import ByteBoard.Tools as Tools


type Model
    = Model ModelData


type alias ModelData =
    { mouse : Position
    , mouseActive : Bool
    , pending : List Position
    , drawings : List Form
    }


type alias Radius =
    Int


type Form
    = Circle Position Radius
    | Line Position Position


mousePos : Model -> Position
mousePos (Model { mouse }) =
    mouse


init : Model
init =
    Model
        { mouse = { x = 0, y = 0 }
        , mouseActive = False
        , pending = []
        , drawings = []
        }


type Msg
    = Abort
    | MouseOn
    | MouseOff
    | Mouse Position
    | Click Tools.Tool


update : Msg -> Model -> Model
update msg ((Model data) as model) =
    case msg of
        Abort ->
            Model { data | pending = [] }

        MouseOn ->
            Model { data | mouseActive = True }

        MouseOff ->
            Model { data | mouseActive = False }

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

        Tools.Circle ->
            List.head pending
                |> Maybe.map (\a -> Circle a (hypotenuse <| delta mouse a))

        Tools.Line ->
            List.head pending
                |> Maybe.map (\a -> Line a (delta mouse a))


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
        , Events.onClick (Click tool)
        , onMouseMove Mouse
        , Events.onMouseEnter MouseOn
        , Events.onMouseLeave MouseOff
        ]
        [ svg [ Attr.width =+ width, Attr.height =+ height ]
            (maybePush (List.map viewForm model.drawings)
                (if model.mouseActive then
                    (Maybe.map viewForm (draw model tool))
                 else
                    Nothing
                )
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
        Circle { x, y } radius ->
            circle
                [ Attr.r =+ radius
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
    Events.on "mousemove" (Json.map msg relativeMousePosition)


relativeMousePosition : Json.Decoder Position
relativeMousePosition =
    Json.object2 Position ("offsetX" := Json.int) ("offsetY" := Json.int)
