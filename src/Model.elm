module Model exposing (Model)

import Day1
import Day10
import Day11
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Time exposing (Posix, Zone)


type alias Model =
    { zone : Zone
    , currentDate : Posix
    , day1 : Day1.Model
    , day2 : Day2.Model
    , day3 : Day3.Model
    , day4 : Day4.Model
    , day5 : Day5.Model
    , day6 : Day6.Model
    , day7 : Day7.Model
    , day8 : Day8.Model
    , day9 : Day9.Model
    , day10 : Day10.Model
    , day11 : Day11.Model
    }
