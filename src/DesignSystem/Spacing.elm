module DesignSystem.Spacing exposing (SpacingSize(..), margin2, marginBottom, marginLeft, marginRight, marginTop, padding2, paddingLeft, paddingTop, spacing)

import Css exposing (Px, Style, px)


type SpacingSize
    = NoSpace
    | XXS
    | XS
    | S
    | M
    | L
    | XL


spacing : SpacingSize -> Px
spacing spacingSize =
    case spacingSize of
        NoSpace ->
            px 0

        XXS ->
            px 4

        XS ->
            px 8

        S ->
            px 16

        M ->
            px 24

        L ->
            px 40

        XL ->
            px 60


marginBottom : SpacingSize -> Style
marginBottom spacingSize =
    Css.marginBottom (spacing spacingSize)


marginTop : SpacingSize -> Style
marginTop spacingSize =
    Css.marginTop (spacing spacingSize)


marginLeft : SpacingSize -> Style
marginLeft spacingSize =
    Css.marginLeft (spacing spacingSize)


marginRight : SpacingSize -> Style
marginRight spacingSize =
    Css.marginRight (spacing spacingSize)


paddingTop : SpacingSize -> Style
paddingTop spacingSize =
    Css.paddingTop (spacing spacingSize)


paddingLeft : SpacingSize -> Style
paddingLeft spacingSize =
    Css.paddingLeft (spacing spacingSize)


padding2 : SpacingSize -> SpacingSize -> Style
padding2 spacingY spacingX =
    Css.padding2 (spacing spacingY) (spacing spacingX)


margin2 : SpacingSize -> SpacingSize -> Style
margin2 spacingY spacingX =
    Css.margin2 (spacing spacingY) (spacing spacingX)
