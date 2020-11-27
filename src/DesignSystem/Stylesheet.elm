module DesignSystem.Stylesheet exposing (..)

import Css.Global exposing (global)
import DesignSystem.Button as Button
import DesignSystem.Global as Global
import DesignSystem.Input as Input
import DesignSystem.Typography as Typography
import Html.Styled


stylesheet : Html.Styled.Html msg
stylesheet =
    global
        (Global.styles
            ++ Button.styles
            ++ Typography.styles
            ++ Input.styles
        )
