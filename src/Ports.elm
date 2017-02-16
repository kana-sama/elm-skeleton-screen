port module Ports exposing (..)

import Scroll


port scroll : (Scroll.Move -> msg) -> Sub msg
