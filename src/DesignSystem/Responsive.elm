module DesignSystem.Responsive exposing (onMobile, onSmallScreen)

import Css exposing (Style, px)
import Css.Media exposing (only, screen)


onSmallScreen : List Style -> Style
onSmallScreen styles =
    Css.Media.withMedia
        [ only screen [ Css.Media.maxWidth (px 768) ]
        ]
        styles


onMobile : List Style -> Style
onMobile styles =
    Css.Media.withMedia
        [ only screen [ Css.Media.maxWidth (px 500) ]
        ]
        styles
