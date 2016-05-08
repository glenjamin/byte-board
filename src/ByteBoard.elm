module ByteBoard (..) where

import Color
import Graphics.Collage as Collage
import Mouse
import Effects exposing (Effects)
import Html exposing (div, button, text)
import Html.Attributes as Attr
import Stuff exposing ((=>), push, tuplemap2)


clicks : Signal Action
clicks =
  Mouse.clicks
    |> Signal.map (always Click)


type alias Model =
  { window : Size
  , mouse : Point
  , forms : List Form
  }


type alias Point =
  ( Int, Int )


type alias Size =
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
  = Click
  | Position Size Point


update : Action -> Model -> ( Model, Effects Action )
update action model =
  ( pureUpdate action model, Effects.none )


pureUpdate : Action -> Model -> Model
pureUpdate action model =
  case action of
    Click ->
      { model | forms = push model.forms (Blob model.mouse) }

    Position pos size ->
      { model | mouse = pos, window = size }


view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    { window, mouse, forms } =
      model

    ( w, h ) =
      window

    ( canvasW, sidebarW ) =
      sidebarWidth w
  in
    div
      []
      [ div
          [ Attr.style [ "width" => toString canvasW, "float" => "left" ]
          ]
          [ viewForms ( canvasW, h ) model.forms
          ]
      , div
          [ Attr.style [ "width" => toString sidebarW ]
          ]
          [ text <| toString ( window, mouse ) ]
      ]


sidebarWidth : Int -> ( Int, Int )
sidebarWidth w =
  ( w - 300, 300 )


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
