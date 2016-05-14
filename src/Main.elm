module Main exposing (main)

import Html.App
import ByteBoard as BB


main : Program Never
main =
    Html.App.program
        { init = BB.init
        , update = BB.update
        , view = BB.view
        , subscriptions = BB.subscriptions
        }
