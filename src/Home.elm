module Home exposing (view)

import Css exposing (alignItems, backgroundColor, center, color, display, displayFlex, flexWrap, fontSize, height, justifyContent, margin, none, pct, px, rem, rgb, textDecoration, width, wrap)
import Css.Global as Css exposing (Snippet)
import Html.Styled exposing (Html, a, div, text)
import Html.Styled.Attributes exposing (class, href)
import Time exposing (Posix, Zone)


days : List Int
days =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 ]


view : Zone -> Posix -> Html msg
view zone currentDate =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    div [ class "days" ]
        (Css.global styles
            :: (days
                    |> List.filter (\day -> day <= maxDay)
                    |> List.map viewDay
               )
        )


viewDay : Int -> Html msg
viewDay day =
    a [ class "day", href "#" ]
        [ text (String.fromInt day)
        ]


styles : List Snippet
styles =
    [ Css.class "days"
        [ displayFlex
        , flexWrap wrap
        ]
    , Css.class "day"
        [ width (px 180)
        , height (px 180)
        , margin (px 10)
        , backgroundColor (rgb 200 56 17)
        , color (rgb 250 250 250)
        , fontSize (rem 3)
        , displayFlex
        , alignItems center
        , justifyContent center
        , textDecoration none
        , Css.hover
            [ backgroundColor (rgb 17 100 56)
            ]
        ]
    ]
