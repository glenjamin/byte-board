module Main exposing (main)

import Mouse
import Window
import Html
import Task
import Html.App
import ByteBoard as BB


main =
  app.html


app =
  Html.App.program
    { init = BB.init, Cmd.none )
    , update = BB.update
    , view = BB.view
    , subscriptions = \_ -> Sub.none
    }


positions : Signal BB.Action
positions =
  Signal.map2 BB.Position Mouse.position Window.dimensions


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


port swap : Signal Bool
