port module Ports exposing (DayState, saveToLocalStorage, stateFromLocalStorage)

import Json.Decode as Decode
import Json.Encode as Encode


type alias DayState =
    { day : Int, model : Encode.Value }


port saveToLocalStorage : DayState -> Cmd msg


port stateFromLocalStorage : (Decode.Value -> msg) -> Sub msg
