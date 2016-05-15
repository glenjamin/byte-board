module ByteBoard exposing (init, subscriptions, update, view)

import Debug
import Json.Decode as Json exposing ((:=))
import Html exposing (Html, p, div, code, br, button, text)
import Html.App as Html
import Html.Events exposing (on, onClick)
import Html.Attributes as Attr
import Stuff exposing ((=>), (??), push, tuplemap2)
import Task
import Window
import ByteBoard.Types exposing (Size, Position)
import ByteBoard.Drawing as Drawing
import ByteBoard.Tools as Tools


type alias Model =
    { window : Size
    , mouse : Position
    , drawing : List Drawing.Form
    , tool : Tools.Tool
    }


init : ( Model, Cmd Msg )
init =
    { window = { width = 0, height = 0 }
    , mouse = { x = 0, y = 0 }
    , drawing = Drawing.init
    , tool = Tools.init
    }
        ! [ initialWindowSize ]


initialWindowSize : Cmd Msg
initialWindowSize =
    Task.perform (\_ -> Debug.crash "no window?")
        WindowSize
        Window.size


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes WindowSize ]


type Msg
    = Click
    | ChangeTool Tools.Tool
    | MousePosition Position
    | WindowSize Size


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    ( pureUpdate action model, Cmd.none )


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        MousePosition pos ->
            { model | mouse = pos }

        ChangeTool tool ->
            { model | tool = tool }

        Click ->
            { model
                | drawing =
                    push model.drawing (Drawing.draw model.tool model.mouse)
            }

        WindowSize size ->
            { model | window = size }


view : Model -> Html Msg
view model =
    let
        { width, height } =
            model.window

        sidebarW =
            300

        canvasSize =
            { height = height, width = max 0 (width - sidebarW) }
    in
        div []
            [ div
                [ Attr.style
                    [ "position" => "absolute"
                    , "left" => "0"
                    , "right" => px sidebarW
                    , "top" => "0"
                    , "bottom" => "0"
                    , "overflow" => "hidden"
                    ]
                ]
                [ div
                    [ onClick Click
                    , onMouseMove MousePosition
                    ]
                    [ Drawing.view canvasSize model.drawing
                    ]
                ]
            , div
                [ Attr.style
                    [ "position" => "absolute"
                    , "top" => "0"
                    , "bottom" => "0"
                    , "width" => px sidebarW
                    , "right" => "0"
                    , "borderLeft" => "1px solid #999"
                    ]
                ]
                [ viewSidebar model ]
            ]


viewSidebar : Model -> Html Msg
viewSidebar { window, mouse, tool } =
    div [ Attr.style [ "padding" => "10px" ] ]
        [ code [] [ text <| toString window ]
        , br [] []
        , code [] [ text <| toString mouse ]
        , Html.map ChangeTool (Tools.view tool)
        ]


px : number -> String
px n =
    (toString n) ++ "px"


onMouseMove : (Position -> Msg) -> Html.Attribute Msg
onMouseMove msg =
    on "mousemove" (Json.map msg relativeMousePosition)


relativeMousePosition : Json.Decoder Position
relativeMousePosition =
    Json.object2 Position ("offsetX" := Json.int) ("offsetY" := Json.int)
