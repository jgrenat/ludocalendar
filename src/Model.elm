module Model exposing (Model)

import Day1
import Day2
import Day3
import Day4
import Day5
import Time exposing (Posix, Zone)


type alias Model =
    { zone : Zone
    , currentDate : Posix
    , day1 : Day1.Model
    , day2 : Day2.Model
    , day3 : Day3.Model
    , day4 : Day4.Model
    , day5 : Day5.Model
    }
