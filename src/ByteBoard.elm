module Main (..) where

import Color
import Graphics.Collage as Collage
import Mouse
import Window
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Html exposing (div, button, text)
import StartApp


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


app : StartApp.App Model
app =
  StartApp.start
    { init = ( init, sendInitial )
    , update = update
    , view = view
    , inputs = [ clicks, windowSize, mousePos ]
    }


initialMailbox : Signal.Mailbox ()
initialMailbox =
  Signal.mailbox ()


initialSignal : Signal ()
initialSignal =
  initialMailbox.signal


sendInitial : Effects Action
sendInitial =
  Signal.send initialMailbox.address ()
    |> Task.map (always Nothing)
    |> Effects.task


clicks : Signal Action
clicks =
  Mouse.clicks
    |> Signal.map (always Click)


mousePos : Signal Action
mousePos =
  Mouse.position
    |> Signal.map2 rehomeMouse Window.dimensions
    |> Signal.map Position


rehomeMouse : ( Int, Int ) -> Point -> Point
rehomeMouse ( w, h ) ( x, y ) =
  ( x - w // 2, h // 2 - y )


windowSize : Signal Action
windowSize =
  Signal.map Window
    <| Signal.merge
        Window.dimensions
        (Signal.sampleOn initialSignal Window.dimensions)


type alias Model =
  { window : Point
  , mouse : Point
  , forms : List Form
  }


type alias Point =
  ( Int, Int )


type Form
  = Blob Point


init : Model
init =
  { window = ( 0, 0 )
  , mouse = ( 0, 0 )
  , forms = [ Blob ( 0, 0 ) ]
  }


type Action
  = Nothing
  | Click
  | Position Point
  | Window ( Int, Int )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  ( pureUpdate action model, Effects.none )


pureUpdate : Action -> Model -> Model
pureUpdate action model =
  case action of
    Nothing ->
      model

    Click ->
      { model | forms = push model.forms (Blob model.mouse) }

    Position pos ->
      { model | mouse = pos }

    Window size ->
      { model | window = size }


view : Signal.Address Action -> Model -> Html.Html
view address { window, mouse, forms } =
  div
    []
    [ text (toString mouse)
    , text (toString window)
    , viewForms window forms
    ]


viewForms : ( Int, Int ) -> List Form -> Html.Html
viewForms ( width, height ) forms =
  forms
    |> List.map viewForm
    |> Collage.collage width height
    |> Html.fromElement


viewForm : Form -> Collage.Form
viewForm form =
  case form of
    Blob pos ->
      Collage.circle 30
        |> Collage.filled Color.red
        |> Collage.move (tuplemap2 toFloat pos)


tuplemap2 : (a -> b) -> ( a, a ) -> ( b, b )
tuplemap2 f ( x, y ) =
  ( f x, f y )


push : List a -> a -> List a
push l x =
  l ++ [ x ]
