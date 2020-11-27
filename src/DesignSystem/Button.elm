module DesignSystem.Button exposing (ButtonSize(..), ButtonType(..), button, buttonLink, buttonWithStatus, styles)

import Css exposing (Color, Style, animationDuration, animationIterationCount, animationName, backgroundColor, border, border3, borderRadius, center, color, cursor, display, em, fontFamilies, height, inlineBlock, int, margin2, none, notAllowed, padding2, pct, pointer, property, px, rem, sec, solid, textAlign, textDecoration, textTransform, transparent, uppercase, width, zero)
import Css.Animations as Animations exposing (keyframes)
import Css.Global as Css exposing (withClass)
import Css.Media as Media
import DesignSystem.Colors as Colors
import DesignSystem.Input exposing (searchInputHeight)
import DesignSystem.Spacing exposing (SpacingSize(..), spacing)
import DesignSystem.Typography as FontSize exposing (fontSize)
import Html.Styled as Html exposing (Html, a, span)
import Html.Styled.Attributes exposing (class, classList, disabled, href)
import RemoteData exposing (RemoteData(..))


type ButtonType
    = Primary
    | Secondary
    | Link


type ButtonSize
    = Small
    | Large
    | SearchInputSize


button : ButtonType -> ButtonSize -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
button buttonType buttonSize attributes content =
    let
        typeClass =
            class (getTypeClass buttonType)

        sizeClass =
            class (getSizeClass buttonSize)
    in
    Html.button
        ([ class "button", typeClass, sizeClass ] ++ attributes)
        content


buttonLink : ButtonType -> ButtonSize -> String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
buttonLink buttonType buttonSize link attributes content =
    let
        typeClass =
            class (getTypeClass buttonType)

        sizeClass =
            class (getSizeClass buttonSize)
    in
    a ([ class "button button--link", sizeClass, typeClass, href link ] ++ attributes)
        content


buttonWithStatus : ButtonType -> ButtonSize -> RemoteData e a -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
buttonWithStatus buttonType buttonSize status attributes content =
    let
        typeClass =
            class (getTypeClass buttonType)

        sizeClass =
            class (getSizeClass buttonSize)
    in
    Html.button
        ([ class "button", classList [ ( "button--loading", RemoteData.isLoading status ) ], typeClass, sizeClass, disabled (status == Loading) ] ++ attributes)
        (case status of
            Loading ->
                List.repeat 3 (span [] [])

            _ ->
                content
        )


getTypeClass : ButtonType -> String
getTypeClass buttonType =
    case buttonType of
        Primary ->
            "button--primary"

        Secondary ->
            "button--secondary"

        Link ->
            "button--linkDisplay"


getSizeClass : ButtonSize -> String
getSizeClass buttonSize =
    case buttonSize of
        Small ->
            "button--small"

        Large ->
            "button--large"

        SearchInputSize ->
            "button--searchInputSize"


styles : List Css.Snippet
styles =
    [ Css.class "button"
        [ fontFamilies [ "Roboto", "sans-serif" ]
        , borderRadius (px 3)
        , cursor pointer
        , textAlign center
        , withClass "button--primary"
            [ backgroundColor Colors.primaryButtonBackground
            , color Colors.primaryButtonText
            , border3 (px 1) solid Colors.primaryButtonBorder
            , Media.withMedia [ Media.all [ Media.hover Media.canHover ] ]
                [ Css.hover
                    [ backgroundColor Colors.primaryButtonHoverBackground
                    ]
                ]
            , Css.active
                [ backgroundColor Colors.primaryButtonActiveBackground
                ]
            ]
        , withClass "button--secondary"
            [ border3 (px 1) solid Colors.secondaryButtonBorder
            , color Colors.secondaryButtonText
            , backgroundColor Colors.secondaryButtonBackground
            , Css.hover
                [ color Colors.secondaryButtonHoverText
                , backgroundColor Colors.secondaryButtonHoverBackground
                ]
            , Css.active
                [ backgroundColor Colors.secondaryButtonActiveBackground
                ]
            ]
        , withClass "button--linkDisplay"
            [ backgroundColor transparent
            , color Colors.typographyMedium
            , fontSize FontSize.S
            , border (px 0)
            ]
        , withClass "button--large"
            [ padding2 (px 15) (px 20)
            , fontSize FontSize.L
            , borderRadius (px 5)
            ]
        , withClass "button--searchInputSize"
            [ height searchInputHeight
            ]
        , withClass "button--loading"
            [ Css.children
                [ Css.span
                    [ display inlineBlock
                    , borderRadius (pct 100)
                    , height (em 0.5)
                    , width (em 0.5)
                    , backgroundColor Colors.deactivatedButtonBackground
                    , margin2 zero (spacing XXS)
                    , animationDuration (sec 1)
                    , animationIterationCount (int 50)
                    , property "animation-timing-function" "ease-out"
                    ]
                ]
            , withClass "button--primary"
                [ Css.children
                    [ Css.span (loadingAnimations Colors.primaryButtonBackground Colors.white)
                    ]
                ]
            , withClass "button--secondary"
                [ Css.children
                    [ Css.span (loadingAnimations Colors.secondaryButtonBackground Colors.primaryButtonBackground)
                    ]
                ]
            ]
        , withClass "button--link"
            [ textDecoration none
            , display inlineBlock
            , property "line-height" "normal"
            ]
        , Css.disabled [ cursor notAllowed ]
        ]
    ]


loadingAnimations : Color -> Color -> List Style
loadingAnimations firstColor secondColor =
    [ Css.nthChild "1"
        [ animationName
            (keyframes
                [ ( 0, [ Animations.backgroundColor firstColor ] )
                , ( 50, [ Animations.backgroundColor secondColor ] )
                , ( 100, [ Animations.backgroundColor secondColor ] )
                ]
            )
        ]
    , Css.nthChild "2"
        [ animationName
            (keyframes
                [ ( 0, [ Animations.backgroundColor firstColor ] )
                , ( 15, [ Animations.backgroundColor firstColor ] )
                , ( 65, [ Animations.backgroundColor secondColor ] )
                , ( 100, [ Animations.backgroundColor secondColor ] )
                ]
            )
        ]
    , Css.nthChild "3"
        [ animationName
            (keyframes
                [ ( 0, [ Animations.backgroundColor firstColor ] )
                , ( 30, [ Animations.backgroundColor firstColor ] )
                , ( 80, [ Animations.backgroundColor secondColor ] )
                , ( 100, [ Animations.backgroundColor secondColor ] )
                ]
            )
        ]
    ]
