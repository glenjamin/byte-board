module Main (..) where

import Color
import Graphics.Collage as Collage
import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp


main : Signal Html.Html
main =
  StartApp.start { model = model, view = view, update = update }


type alias Model =
  Float


model : Model
model =
  0


view : Signal.Address Action -> Model -> Html.Html
view address model =
  div
    []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "+" ]
    , Html.fromElement <| Collage.collage 500 500 <| drawings model
    ]


drawings : Model -> List Collage.Form
drawings model =
  [1..model]
    |> List.map line


line : Float -> Collage.Form
line offset =
  Collage.path [ ( 0, offset * 10 ), ( 500, offset * 10 ) ]
    |> Collage.traced (Collage.solid Color.red)


type Action
  = Increment
  | Decrement


update : Action -> Model -> Model
update action model =
  case action of
    Increment ->
      model + 1

    Decrement ->
      model - 1
