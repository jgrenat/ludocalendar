module Model exposing (Model)

import Day1
import Time exposing (Posix, Zone)


type alias Model =
    { zone : Zone, currentDate : Posix, day1 : Day1.Model }
