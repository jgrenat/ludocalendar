module Utils.Html exposing (AriaRole(..), ariaControls, ariaExpanded, ariaHasPopup, ariaLabelledBy, nonBreakableSpace, nothing, role, screenReaderOnly, viewIf, viewIfLazy, viewMaybe)

import Css exposing (absolute, height, hidden, left, overflow, position, px, width)
import Html.Styled as Html exposing (Attribute, Html, span, text)
import Html.Styled.Attributes exposing (attribute, css)


nothing : Html msg
nothing =
    Html.text ""


viewIf : Bool -> Html msg -> Html msg
viewIf condition html =
    if condition then
        html

    else
        nothing


viewIfLazy : Bool -> (() -> Html msg) -> Html msg
viewIfLazy condition htmlF =
    if condition then
        htmlF ()

    else
        nothing


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe fn maybeThing =
    maybeThing
        |> Maybe.map fn
        |> Maybe.withDefault nothing


nonBreakableSpace : String
nonBreakableSpace =
    "\u{00A0}"



---- Aria ----


type AriaRole
    = PresentationRole
    | ButtonRole
    | MenuRole
    | MenuItemRole


role : AriaRole -> Attribute msg
role =
    ariaRoleToString >> attribute "role"


ariaHasPopup : Bool -> Attribute msg
ariaHasPopup value =
    attribute "aria-haspopup" (boolToString value)


ariaExpanded : Bool -> Attribute msg
ariaExpanded value =
    attribute "aria-expanded" (boolToString value)


ariaControls : String -> Attribute msg
ariaControls elementId =
    attribute "aria-controls" elementId


ariaLabelledBy : String -> Attribute msg
ariaLabelledBy elementId =
    attribute "aria-labelledby" elementId


ariaRoleToString : AriaRole -> String
ariaRoleToString ariaRole =
    case ariaRole of
        PresentationRole ->
            "presentation"

        ButtonRole ->
            "button"

        MenuRole ->
            "menu"

        MenuItemRole ->
            "menuitem"


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"


screenReaderOnly : String -> Html msg
screenReaderOnly content =
    span
        [ css
            [ position absolute
            , left (px -10000)
            , width (px 1)
            , height (px 1)
            , overflow hidden
            ]
        ]
        [ text content ]
