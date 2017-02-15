module Main exposing (..)

import App
import Html exposing (Html)
import Time


main : Program Never App.Model App.Msg
main =
    Html.program
        { view = App.view
        , init = App.init Time.second
        , update = App.update
        , subscriptions = App.subscriptions
        }
