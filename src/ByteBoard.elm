module ByteBoard exposing (init, subscriptions, update, view)

import Debug
import Html exposing (Html, p, div, code, br, button, text)
import Html.App as Html
import Html.Events exposing (on, onClick)
import Html.Attributes as Attr
import Stuff exposing ((=>), (??), tuplemap2)
import Task
import Window
import ByteBoard.Types exposing (Size, Position)
import ByteBoard.Drawing as Drawing
import ByteBoard.Tools as Tools
import Native.Something


type alias Model =
    { window : Size
    , drawing : Drawing.Model
    , tool : Tools.Tool
    }


init : ( Model, Cmd Msg )
init =
    { window = { width = 0, height = 0 }
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
    let
        _ =
            Debug.log "native" Native.Something.whatever
    in
        Sub.batch [ Window.resizes WindowSize ]


type Msg
    = Clear
    | ChangeTool Tools.Tool
    | DrawingMsg Drawing.Msg
    | WindowSize Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( pureUpdate msg model, Cmd.none )


pureUpdate : Msg -> Model -> Model
pureUpdate msg ({ tool, drawing } as model) =
    case msg of
        Clear ->
            { model | drawing = Drawing.init }

        ChangeTool tool ->
            { model
                | tool = tool
                , drawing = Drawing.update Drawing.Abort drawing
            }

        DrawingMsg drawingMsg ->
            { model | drawing = Drawing.update drawingMsg drawing }

        WindowSize size ->
            { model | window = size }


view : Model -> Html Msg
view ({ tool, drawing, window } as model) =
    let
        { width, height } =
            window

        sidebarW =
            200

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
                [ Html.map DrawingMsg
                    (Drawing.view canvasSize tool drawing)
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
viewSidebar { drawing, tool } =
    let
        { x, y } =
            Drawing.mousePos drawing
    in
        div [ Attr.style [ "padding" => "10px" ] ]
            [ code [ Attr.style [ "display" => "block" ] ]
                [ text <| toString ( x, y ) ]
            , button [ onClick Clear ] [ text "Clear" ]
            , Html.map ChangeTool (Tools.view tool)
            ]


px : number -> String
px n =
    (toString n) ++ "px"
