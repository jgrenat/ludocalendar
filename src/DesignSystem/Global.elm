module DesignSystem.Global exposing (styles)

import Css exposing (auto, backgroundAttachment, backgroundColor, backgroundImage, backgroundPosition, backgroundSize, block, borderRadius, boxShadow, boxShadow5, center, contain, cover, display, fixed, fontStyle, fontWeight, height, int, italic, margin, maxWidth, minHeight, none, pct, px, rgba, textAlign, textDecoration, underline, url, width)
import Css.Global as Css exposing (Snippet, em)
import DesignSystem.Button exposing (ButtonSize(..))
import DesignSystem.Colors as Colors
import DesignSystem.Spacing exposing (SpacingSize(..), marginBottom, marginTop, padding2)
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
