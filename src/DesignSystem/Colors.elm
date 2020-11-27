module DesignSystem.Colors exposing (..)

import Css exposing (Color, ColorValue, NonMixable, hex, rgb, rgba, transparent)


green : Color
green =
    hex "#2ca58d"


yellow : Color
yellow =
    hex "#F6F1D1"


white : Color
white =
    rgb 255 255 255


facebookBlue : Color
facebookBlue =
    hex "3B5998"



-- Global


primary : Color
primary =
    hex "2C99B0"


secondary : Color
secondary =
    hex "423783"


containerBackground : Color
containerBackground =
    rgba 255 255 255 0.95


containerShadow : Color
containerShadow =
    rgba 0 0 0 0.75


panelColor : Color
panelColor =
    hex "F6B800"


panelLinkColor : Color
panelLinkColor =
    hex "F9C103"



-- Typography


typographyHigh : Color
typographyHigh =
    rgba 0 0 0 0.87


typographyMedium : Color
typographyMedium =
    rgba 0 0 0 0.6



---- Fields ----


fieldBackground : Color
fieldBackground =
    rgb 255 255 255


fieldBorder : Color
fieldBorder =
    rgba 0 0 0 0.5


fieldFocus : Color
fieldFocus =
    rgba 0 0 30 0.5


fieldBorderHover : Color
fieldBorderHover =
    rgba 0 0 0 0.5


fieldBorderError : Color
fieldBorderError =
    hex "b00020"


fieldError : Color
fieldError =
    hex "b00020"


fieldLabelText : Color
fieldLabelText =
    rgba 0 0 0 0.6


fieldInputText : Color
fieldInputText =
    rgba 0 0 0 0.38


fieldInputTextHoverActive : Color
fieldInputTextHoverActive =
    rgba 0 0 0 0.6


fieldInputTextFocused : Color
fieldInputTextFocused =
    rgba 0 0 0 0.52


radioInputBackgroundColor : ColorValue NonMixable
radioInputBackgroundColor =
    transparent


radioInputBorder : Color
radioInputBorder =
    hex "E5E5E5"


checkedRadioInputBackground : Color
checkedRadioInputBackground =
    hex "023466"


checkedRadioInputBorder : Color
checkedRadioInputBorder =
    checkedRadioInputBackground


horizontalSelectBackgroud : Color
horizontalSelectBackgroud =
    rgba 0 0 0 0.12



-- Buttons


primaryButtonBackground : Color
primaryButtonBackground =
    green


primaryButtonBorder : Color
primaryButtonBorder =
    green


primaryButtonText : Color
primaryButtonText =
    white


primaryButtonHoverBackground : Color
primaryButtonHoverBackground =
    rgba 0 0 0 0.8


primaryButtonActiveBackground : Color
primaryButtonActiveBackground =
    rgba 0 0 0 0.73


secondaryButtonText : Color
secondaryButtonText =
    rgba 0 0 0 0.6


secondaryButtonBorder : Color
secondaryButtonBorder =
    rgba 0 0 0 0.6


secondaryButtonBackground : Color
secondaryButtonBackground =
    rgb 255 255 255


secondaryButtonHoverText : Color
secondaryButtonHoverText =
    white


secondaryButtonHoverBackground : Color
secondaryButtonHoverBackground =
    primary


secondaryButtonActiveBackground : Color
secondaryButtonActiveBackground =
    secondary


deactivatedButtonBackground : Color
deactivatedButtonBackground =
    rgba 0 0 0 0.2
