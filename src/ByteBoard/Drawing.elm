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
import Svg exposing (Svg, svg, text, path, circle, rect)
import Svg.Attributes as Attr
import Stuff exposing (push, maybePush)
import ByteBoard.Types
    exposing
        ( Size
        , Position
        , Bounds
        , delta
        , hypotenuse
        , bounds
        )
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
    = Rectangle Bounds
    | Circle Position Radius
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
    let
        points =
            push pending mouse
    in
        case tool of
            Tools.Select ->
                Nothing

            Tools.Rectangle ->
                twoPoints points (\a b -> Rectangle (bounds a b))

            Tools.Circle ->
                twoPoints points (\a b -> Circle a (hypotenuse <| delta b a))

            Tools.Line ->
                twoPoints points (\a b -> Line a (delta b a))


twoPoints : List Position -> (Position -> Position -> Form) -> Maybe Form
twoPoints points f =
    case List.take 2 points of
        [ a, b ] ->
            Just (f a b)

        _ ->
            Nothing


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


coloured : (String -> Svg.Attribute msg) -> Color -> Svg.Attribute msg
coloured attr value =
    attr (colorToCssRgba value)


viewForm : Form -> Svg msg
viewForm form =
    case form of
        Rectangle bounds ->
            rect
                [ Attr.x =+ bounds.x
                , Attr.y =+ bounds.y
                , Attr.width =+ bounds.dx
                , Attr.height =+ bounds.dy
                , Attr.fill 🖌 Color.orange
                ]
                []

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
                , Attr.strokeWidth =+ 10
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
