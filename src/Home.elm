module Home exposing (view)

import Css exposing (absolute, backgroundColor, center, color, displayFlex, em, flexWrap, fontSize, height, justifyContent, left, margin, none, pct, position, px, relative, rem, rgb, right, textAlign, textDecoration, top, transforms, translateX, translateY, width, wrap)
import Css.Global as Css exposing (Snippet)
import Css.Media as Media
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import DesignSystem.Spacing as Spacing exposing (marginTop, paddingLeft, paddingRight)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, a, aside, br, div, p, span, text)
import Html.Styled.Attributes exposing (class, css, href)
import Model exposing (Model)
import Pages exposing (pages)
import Pages.PagePath as PagePath exposing (PagePath)
import Time exposing (Posix, Zone)
import Utils.Html exposing (attributeIf)


view : Model -> Html msg
view model =
    let
        days =
            [ ( 1, Day1.isDone model.day1, pages.day1 )
            , ( 2, Day2.isDone model.day2, pages.day2 )
            , ( 3, Day3.isDone model.day3, pages.day3 )
            , ( 4, Day4.isDone model.day4, pages.day4 )
            , ( 5, Day5.isDone model.day5, pages.day5 )
            , ( 6, Day6.isDone model.day6, pages.day6 )
            , ( 7, Day7.isDone model.day7, pages.day7 )
            , ( 8, Day8.isDone model.day8, pages.day8 )
            , ( 9, Day9.isDone model.day9, pages.day9 )
            , ( 10, False, pages.day2 )
            , ( 11, False, pages.day2 )
            , ( 12, False, pages.day2 )
            , ( 13, False, pages.day2 )
            , ( 14, False, pages.day2 )
            , ( 15, False, pages.day2 )
            , ( 16, False, pages.day2 )
            , ( 17, False, pages.day2 )
            , ( 18, False, pages.day2 )
            , ( 19, False, pages.day2 )
            , ( 20, False, pages.day2 )
            , ( 21, False, pages.day2 )
            , ( 22, False, pages.day2 )
            , ( 23, False, pages.day2 )
            , ( 24, False, pages.day2 )
            ]

        maxDay =
            if Time.toYear model.zone model.currentDate > 2020 then
                24

            else
                Time.toDay model.zone model.currentDate
    in
    div [ class "days" ]
        ((Css.global styles
            :: (days
                    |> List.filter (\( day, _, _ ) -> day <= maxDay)
                    |> List.map viewDay
               )
         )
            ++ [ aside [ css [ marginTop Spacing.XL, textAlign center, paddingLeft Spacing.XL, paddingRight Spacing.XL ] ]
                    [ typography Paragraph p [] "LudoCalendar est l'initiative bénévole d'un passionné de jeux de société."
                    , p [ css [ marginTop Spacing.M ] ]
                        [ typography Paragraph a [ href "mailto:contact@ludocalendar.com" ] "Une idée, une envie, besoin d'aide ? Contactez-nous !"
                        ]
                    ]
               ]
        )


viewDay : ( Int, Bool, PagePath pathKey ) -> Html msg
viewDay ( day, isDone, path ) =
    a [ class "day", attributeIf isDone (class "day--done"), href (PagePath.toString path) ]
        [ div [ class "text" ] [ text (String.fromInt day) ]
        ]


styles : List Snippet
styles =
    [ Css.class "days"
        [ displayFlex
        , flexWrap wrap
        , justifyContent center
        ]
    , Css.class "day"
        [ width (px 180)
        , height (px 180)
        , Media.withMedia [ Media.all [ Media.maxWidth (px 400) ] ]
            [ width (px 100)
            , height (px 100)
            , fontSize (rem 2)
            , margin (px 5)
            ]
        , Media.withMedia [ Media.all [ Media.maxWidth (px 700) ] ]
            [ width (px 130)
            , height (px 130)
            , fontSize (rem 2)
            , margin (px 10)
            ]
        , margin (px 10)
        , backgroundColor (rgb 200 56 17)
        , color (rgb 250 250 250)
        , fontSize (rem 3)
        , textDecoration none
        , position relative
        , Css.children
            [ Css.class "text"
                [ position absolute
                , top (pct 50)
                , left (pct 50)
                , transforms [ translateX (pct -50), translateY (pct -50) ]
                ]
            ]
        , Css.hover
            [ backgroundColor (rgb 17 100 56)
            ]
        , Css.withClass "day--done"
            [ Css.after
                [ Css.property "content" "'✓'"
                , position absolute
                , right (px 10)
                , top (px 10)
                , fontSize (em 0.6)
                ]
            ]
        ]
    ]
