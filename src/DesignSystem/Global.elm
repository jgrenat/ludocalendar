module DesignSystem.Global exposing (styles)

import Css exposing (auto, backgroundAttachment, backgroundColor, backgroundImage, backgroundPosition, backgroundSize, block, border3, borderColor, borderRadius, boxShadow, boxShadow5, center, color, contain, cover, display, fixed, fontStyle, fontWeight, height, hex, int, italic, margin, maxWidth, minHeight, none, outline, pct, px, rgb, rgba, solid, textAlign, textDecoration, underline, url, width, zero)
import Css.Global as Css exposing (Snippet, em)
import Css.Media as Media
import DesignSystem.Button exposing (ButtonSize(..))
import DesignSystem.Colors as Colors
import DesignSystem.Spacing as Spacing exposing (SpacingSize(..), marginBottom, marginTop, padding, padding2)
import DesignSystem.Typography as FontSize exposing (FontFamily(..), fontFamily, fontSize)


styles : List Snippet
styles =
    [ Css.html
        [ height (pct 100)
        ]
    , Css.body
        [ height (pct 100)
        , fontFamily Lato
        , backgroundImage (url "/images/background.svg")
        , backgroundSize cover
        , backgroundAttachment fixed
        ]
    , Css.header
        [ textAlign center
        , display block
        , marginBottom M
        , Css.children [ Css.a [ textDecoration none ] ]
        ]
    , Css.class "footer"
        [ textAlign center
        , marginTop L
        , marginBottom M
        ]
    , Css.a
        [ textDecoration underline
        , Css.hover [ textDecoration none ]
        ]
    , Css.class "container"
        [ maxWidth (pct 100)
        , width (px 848)
        , margin auto
        , backgroundColor Colors.containerBackground
        , padding2 M M
        , minHeight (pct 100)
        , boxShadow5 (px 0) (px 1) (px 20) (px -4) Colors.containerShadow
        , Media.withMedia [ Media.all [ Media.maxWidth (px 500) ] ]
            [ padding2 XS XS
            ]
        ]
    , Css.class "catchPhrase"
        [ textAlign center ]
    , Css.class "panel"
        [ backgroundColor Colors.panelColor
        , padding2 S S
        , boxShadow5 (px 0) (px 1) (px 10) (px -4) Colors.containerShadow
        , borderRadius (px 10)
        ]
    , Css.a
        [ Css.children
            [ Css.class "panel"
                [ Css.hover [ backgroundColor Colors.panelLinkColor ]
                , textDecoration none
                ]
            ]
        ]
    , Css.input
        [ borderColor Colors.primary
        , padding Spacing.XS
        , Css.focus
            [ outline none
            , backgroundColor (hex "ddFFdd")
            ]
        ]
    , Css.button
        [ color (rgb 250 250 250)
        , borderRadius zero
        , border3 (px 2) solid Colors.primary
        , backgroundColor Colors.primary
        , padding Spacing.XS
        , Css.focus
            [ outline none
            , borderColor (hex "226622")
            , backgroundColor (hex "226622")
            ]
        ]
    , Css.typeSelector "block-content"
        [ Css.descendants
            [ Css.strong
                [ fontWeight (int 900)
                ]
            , em
                [ fontStyle italic
                ]
            , Css.a
                [ textDecoration underline
                , Css.hover [ textDecoration none ]
                ]
            ]
        ]
    ]
