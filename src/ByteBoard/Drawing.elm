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
    { pending : Maybe (List Position)
    , drawings : List Form
    }


type Form
    = Blob Position
    | Line Position Position


init : Model
init =
    { pending = Nothing
    , drawings = [ Blob { x = 100, y = 100 } ]
    }


type Msg
    = Click Tools.Tool Position


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click tool pos ->
            case tool of
                Tools.Blob ->
                    drawBlob pos model

                Tools.Line ->
                    drawLine pos model


drawBlob : Position -> Model -> Model
drawBlob pos model =
    { model
        | pending = Nothing
        , drawings = push model.drawings (Blob pos)
    }


drawLine : Position -> Model -> Model
drawLine pos model =
    case model.pending of
        Nothing ->
            { model | pending = Just [ pos ] }

        Just points ->
            { model
                | pending = Nothing
                , drawings = maybePush model.drawings (finishLine pos points)
            }


finishLine : Position -> List Position -> Maybe Form
finishLine pos points =
    points
        |> List.head
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


view : Size -> Position -> Model -> Html msg
view { width, height } mouse { pending, drawings } =
    div [ Attr.style canvasStyle ]
        [ svg [ Attr.width =+ width, Attr.height =+ height ]
            (List.map viewForm drawings
                |> (flip maybePush (viewPending pending mouse))
            )
        ]


viewPending : Maybe (List Position) -> Position -> Maybe (Svg msg)
viewPending points pos =
    Maybe.andThen points (finishLine pos)
        |> Maybe.map viewForm


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
