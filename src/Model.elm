module Model exposing (Model)

import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day2
import Day20
import Day21
import Day22
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
    , day12 : Day12.Model
    , day13 : Day13.Model
    , day14 : Day14.Model
    , day15 : Day15.Model
    , day16 : Day16.Model
    , day17 : Day17.Model
    , day18 : Day18.Model
    , day19 : Day19.Model
    , day20 : Day20.Model
    , day21 : Day21.Model
    , day22 : Day22.Model
    }
