module DesignSystem.Global exposing (styles)

import Css exposing (auto, backgroundAttachment, backgroundColor, backgroundImage, backgroundSize, block, border3, borderColor, borderRadius, borderStyle, boxShadow5, center, color, cover, cursor, dashed, default, display, fixed, fontStyle, fontWeight, height, hex, int, italic, margin, maxWidth, minHeight, none, outline, pct, pointer, px, rgb, solid, textAlign, textDecoration, transparent, underline, url, width, zero)
import Css.Global as Global exposing (Snippet)
import Css.Media as Media
import DesignSystem.Colors as Colors
import DesignSystem.Spacing as Spacing exposing (SpacingSize(..), marginBottom, marginTop, padding, padding2)
import DesignSystem.Typography exposing (FontFamily(..), fontFamily)


styles : List Snippet
styles =
    [ Global.html
        [ height (pct 100)
        ]
    , Global.body
        [ height (pct 100)
        , fontFamily Lato
        , backgroundImage (url "/images/background.svg")
        , backgroundSize cover
        , backgroundAttachment fixed
        ]
    , Global.header
        [ textAlign center
        , display block
        , marginBottom M
        , Global.children [ Global.a [ textDecoration none ] ]
        ]
    , Global.class "footer"
        [ textAlign center
        , marginTop L
        , marginBottom M
        ]
    , Global.a
        [ textDecoration underline
        , Css.hover [ textDecoration none ]
        ]
    , Global.class "container"
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
    , Global.class "catchPhrase"
        [ textAlign center ]
    , Global.class "panel"
        [ backgroundColor Colors.panelColor
        , padding2 S S
        , boxShadow5 (px 0) (px 1) (px 10) (px -4) Colors.containerShadow
        , borderRadius (px 10)
        ]
    , Global.a
        [ Global.children
            [ Global.class "panel"
                [ Css.hover [ backgroundColor Colors.panelLinkColor ]
                , textDecoration none
                ]
            ]
        ]
    , Global.input
        [ borderColor Colors.primary
        , padding Spacing.XS
        , Css.focus
            [ outline none
            , backgroundColor (hex "ddFFdd")
            ]
        ]
    , Global.button
        [ color (rgb 250 250 250)
        , borderRadius zero
        , border3 (px 2) solid Colors.primary
        , backgroundColor Colors.primary
        , padding Spacing.XS
        , cursor pointer
        , Css.disabled [ cursor default ]
        , Css.focus
            [ outline none
            , borderColor (hex "226622")
            , backgroundColor (hex "226622")
            ]
        , Global.withClass "button--secondary"
            [ border3 (px 1) solid (rgb 0 0 0)
            , backgroundColor transparent
            , color (rgb 0 0 0)
            , cursor pointer
            , Css.hover
                [ backgroundColor (rgb 220 220 220)
                ]
            , Css.focus
                [ borderStyle dashed
                ]
            ]
        ]
    , Global.typeSelector "block-content"
        [ Global.descendants
            [ Global.strong
                [ fontWeight (int 900)
                ]
            , Global.em
                [ fontStyle italic
                ]
            , Global.a
                [ textDecoration underline
                , Css.hover [ textDecoration none ]
                ]
            ]
        ]
    ]
