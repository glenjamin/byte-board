module Main (main) where

import Mouse
import Window
import Html
import Effects exposing (Effects, Never)
import Task
import StartApp
import ByteBoard as BB


main : Signal Html.Html
main =
  app.html


app : StartApp.App BB.Model
app =
  StartApp.start
    { init = ( BB.init, Effects.none )
    , update = BB.update
    , view = BB.view
    , inputs = [ positions ]
    }


positions : Signal BB.Action
positions =
  Signal.map2 BB.Position Mouse.position Window.dimensions


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


port swap : Signal Bool
