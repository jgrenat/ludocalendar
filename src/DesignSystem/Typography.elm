module DesignSystem.Typography exposing (FontFamily(..), FontSize(..), TypographyType(..), fontFamily, fontSize, getStylesFor, styles, typography)

import Css exposing (..)
import Css.Global as Css
import DesignSystem.Colors as Colors
import DesignSystem.Responsive exposing (onSmallScreen)
import Html.Styled as Html exposing (Attribute, Html, text)
import Html.Styled.Attributes exposing (class)


type TypographyType
    = Paragraph
    | DateTime
    | Instructions
    | Title1
    | Title2
    | MainTitleFirstPart
    | MainTitleSecondPart
    | HeroText


type FontSize
    = XS
    | S
    | M
    | L
    | XL
    | XXL


type FontFamily
    = Lato
    | MainTitleFont


fontSize : FontSize -> Style
fontSize size =
    case size of
        XS ->
            Css.fontSize (rem 0.6)

        S ->
            Css.fontSize (rem 0.8)

        M ->
            Css.fontSize (rem 1)

        L ->
            Css.fontSize (rem 1.5)

        XL ->
            Css.fontSize (rem 2.5)

        XXL ->
            Css.fontSize (rem 3.5)


fontFamily : FontFamily -> Style
fontFamily family =
    case family of
        Lato ->
            Css.fontFamilies [ "Lato", "Trebuchet MS", "Lucida Grande", "Bitstream Vera Sans", "Helvetica Neue", "sans-serif" ]

        MainTitleFont ->
            Css.fontFamilies [ "Berkshire Swash", "Trebuchet MS", "Lucida Grande", "Bitstream Vera Sans", "Helvetica Neue", "sans-serif" ]


typography : TypographyType -> (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List (Html.Attribute msg) -> String -> Html msg
typography typographyType tagFunction attributes content =
    let
        className =
            getClassName typographyType
    in
    tagFunction (class className :: attributes) [ text content ]


getClassName : TypographyType -> String
getClassName typographyType =
    case typographyType of
        Paragraph ->
            "paragraph"

        MainTitleFirstPart ->
            "mainTitle--firstPart"

        MainTitleSecondPart ->
            "mainTitle--secondPart"

        Title1 ->
            "title1"

        Title2 ->
            "title2"

        DateTime ->
            "dateTime"

        HeroText ->
            "heroText"

        Instructions ->
            "quizBestResultText"


getStylesFor : TypographyType -> List Style
getStylesFor typographyType =
    case typographyType of
        Paragraph ->
            [ fontSize M
            ]

        MainTitleFirstPart ->
            color Colors.primary :: mainTitleStyles

        MainTitleSecondPart ->
            color Colors.secondary :: mainTitleStyles

        Title1 ->
            [ fontSize XL
            , fontWeight (int 500)
            , fontFamily Lato
            ]

        Title2 ->
            [ fontSize L
            , fontWeight (int 500)
            , fontFamily Lato
            ]

        DateTime ->
            [ fontSize S
            , fontStyle italic
            ]

        HeroText ->
            [ fontSize L
            , fontWeight (int 500)
            ]

        Instructions ->
            [ fontStyle italic
            , fontSize M
            ]


mainTitleStyles : List Style
mainTitleStyles =
    [ fontSize XXL
    , fontWeight (int 900)
    , fontFamily MainTitleFont
    , onSmallScreen [ fontSize XL ]
    ]


styles : List Css.Snippet
styles =
    [ Paragraph, Title1, Title2, MainTitleFirstPart, MainTitleSecondPart, DateTime, HeroText, Instructions ]
        |> List.map (\typographyType -> Css.class (getClassName typographyType) (getStylesFor typographyType))
