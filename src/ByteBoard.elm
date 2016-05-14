module ByteBoard exposing (init, subscriptions, update, view)

import Debug
import Json.Decode as Json exposing ((:=))
import Html exposing (Html, p, div, button, text)
import Html.Events exposing (on, onClick)
import Html.Attributes as Attr
import Stuff exposing ((=>), (??), push, tuplemap2)
import Task
import Window
import ByteBoard.Drawing exposing (Form(..), viewForms)


type alias Model =
    { window : Size
    , mouse : Position
    , forms : List Form
    }


type alias Position =
    { x : Int, y : Int }


type alias Size =
    Window.Size


init : ( Model, Cmd Msg )
init =
    { window = { width = 0, height = 0 }
    , mouse = { x = 0, y = 0 }
    , forms = [ Blob { x = -100, y = 100 } ]
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

        Click ->
            { model | forms = push model.forms (Blob model.mouse) }

        WindowSize size ->
            { model | window = size }


view : Model -> Html Msg
view model =
    let
        { window, mouse, forms } =
            model

        { width, height } =
            window

        sidebarW =
            300

        canvasW =
            width - sidebarW
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
                    [ (width > 0)
                        ?? viewForms ( canvasW, height ) model.forms
                    ]
                ]
            , div
                [ Attr.style
                    [ "position" => "absolute"
                    , "top" => "0"
                    , "bottom" => "0"
                    , "width" => px sidebarW
                    , "right" => "0"
                    ]
                ]
                [ viewSidebar model ]
            ]


viewSidebar : Model -> Html Msg
viewSidebar { window, mouse } =
    div []
        [ p [] [ text <| toString window ]
        , p [] [ text <| toString mouse ]
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
