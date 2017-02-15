module Main exposing (..)

import App
import Html exposing (Html)


main : Program Never App.Model App.Msg
main =
    Html.program
        { view = App.view
        , init = App.init
        , update = App.update
        , subscriptions = App.subscriptions
        }
