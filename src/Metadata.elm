module Metadata exposing (Metadata(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Metadata
    = Home
    | Day1


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

                    _ ->
                        Decode.fail ("Unexpected page type " ++ pageType)
            )
