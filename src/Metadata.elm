module Metadata exposing (Metadata(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Metadata
    = Home
    | Day1
    | Day2
    | Day3
    | Day4
    | Day5
    | Day6
    | Day7
    | Day8
    | Day9
    | Day10
    | Day11


decoder : Decoder Metadata
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\pageType ->
                case pageType of
                    "home" ->
                        Decode.succeed Home

                    "day1" ->
                        Decode.succeed Day1

                    "day2" ->
                        Decode.succeed Day2

                    "day3" ->
                        Decode.succeed Day3

                    "day4" ->
                        Decode.succeed Day4

                    "day5" ->
                        Decode.succeed Day5

                    "day6" ->
                        Decode.succeed Day6

                    "day7" ->
                        Decode.succeed Day7

                    "day8" ->
                        Decode.succeed Day8

                    "day9" ->
                        Decode.succeed Day9

                    "day10" ->
                        Decode.succeed Day10

                    "day11" ->
                        Decode.succeed Day11

                    _ ->
                        Decode.fail ("Unexpected page type " ++ pageType)
            )
