module Home exposing (view)

import Css exposing (absolute, alignItems, backgroundColor, bottom, center, color, display, displayFlex, em, flexWrap, fontSize, height, justifyContent, margin, none, pct, position, px, relative, rem, rgb, right, textDecoration, top, width, wrap)
import Css.Global as Css exposing (Snippet)
import Day1
import Html.Styled exposing (Html, a, div, text)
import Html.Styled.Attributes exposing (class, href)
import Model exposing (Model)
import Pages exposing (pages)
import Pages.PagePath as PagePath
import Time exposing (Posix, Zone)
import Utils.Html exposing (attributeIf)


view : Model -> Html msg
view model =
    let
        days =
            [ ( 1, Day1.isDone model.day1 )
            , ( 2, False )
            , ( 3, False )
            , ( 4, False )
            , ( 5, False )
            , ( 6, False )
            , ( 7, False )
            , ( 8, False )
            , ( 9, False )
            , ( 10, False )
            , ( 11, False )
            , ( 12, False )
            , ( 14, False )
            , ( 15, False )
            , ( 16, False )
            , ( 17, False )
            , ( 18, False )
            , ( 19, False )
            , ( 20, False )
            , ( 21, False )
            , ( 22, False )
            , ( 23, False )
            , ( 24, False )
            ]

        maxDay =
            if Time.toYear model.zone model.currentDate > 2020 then
                24

            else
                Time.toDay model.zone model.currentDate - 23
    in
    div [ class "days" ]
        (Css.global styles
            :: (days
                    |> List.filter (\( day, _ ) -> day <= maxDay)
                    |> List.map viewDay
               )
        )


viewDay : ( Int, Bool ) -> Html msg
viewDay ( day, isDone ) =
    a [ class "day", attributeIf isDone (class "day--done"), href (PagePath.toString pages.day1) ]
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
        , position relative
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
