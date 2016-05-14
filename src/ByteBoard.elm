module ByteBoard exposing (..)

import Color
import Collage
import Element
import Mouse
import Json.Decode as Json
import Html exposing (Html, p, div, button, text)
import Html.Events exposing (on, onClick)
import Html.Attributes as Attr
import Stuff exposing ((=>), push, tuplemap2)
import Task
import Window


type alias Model =
    { window : Size
    , mouse : Position
    , forms : List Form
    }


type alias Position =
    Mouse.Position


type alias Size =
    Window.Size


type Form
    = Blob Position


init : ( Model, Cmd Msg )
init =
    { window = { width = 0, height = 0 }
    , mouse = { x = 0, y = 0 }
    , forms = [ Blob { x = 0, y = 0 } ]
    }
        ! [ Task.perform ignore1 WindowSize Window.size
          ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes WindowSize ]


ignore1 : a -> Msg
ignore1 _ =
    Ignore


type Msg
    = Ignore
    | Click
    | MousePosition Position
    | WindowSize Size


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    ( pureUpdate action model, Cmd.none )


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        Ignore ->
            model

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

        ( canvasW, sidebarW ) =
            sidebarWidth width
    in
        div []
            [ div
                [ Attr.style [ "width" => toString canvasW, "float" => "left" ]
                , onClick Click
                , onMouseMove MousePosition
                ]
                [ viewForms ( canvasW, height ) model.forms
                ]
            , div
                [ Attr.style [ "width" => toString sidebarW ]
                ]
                [ p [] [ text <| toString window ]
                , p [] [ text <| toString mouse ]
                ]
            ]


onMouseMove : (Position -> Msg) -> Html.Attribute Msg
onMouseMove msg =
    on "mousemove" (Json.map msg Mouse.position)


sidebarWidth : Int -> ( Int, Int )
sidebarWidth w =
    ( w - 300, 300 )


viewForms : ( Int, Int ) -> List Form -> Html Msg
viewForms ( width, height ) forms =
    forms
        |> List.map viewForm
        |> Collage.collage width height
        |> Element.toHtml


viewForm : Form -> Collage.Form
viewForm form =
    case form of
        Blob { x, y } ->
            Collage.circle 30
                |> Collage.filled Color.red
                |> Collage.move ( toFloat x, toFloat y )
