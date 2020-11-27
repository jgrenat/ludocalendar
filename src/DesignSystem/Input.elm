module DesignSystem.Input exposing (Autofocus(..), CheckboxInputConfig, CheckboxModel, InputConfig, InputModel, Required(..), SearchInputModel, checkbox, checkboxModel, commonInputStyles, emptyInput, emptySearchInput, focus, haveErrors, inputId, inputWithLabel, isChecked, prefilledInput, prefilledSearchInput, searchInput, searchInputHeight, searchValue, staticValue, styles, textareaWithLabel, touch, value)

import Browser.Dom as Dom
import Css exposing (Px, Style, absolute, backgroundColor, backgroundImage, backgroundSize, block, border3, borderBox, borderColor, borderRadius, bottom, boxShadow4, boxShadow5, boxSizing, color, contain, content, cursor, display, height, int, left, marginBottom, marginTop, minHeight, none, opacity, outlineColor, padding, padding2, padding4, paddingBottom, paddingRight, pct, pointer, pointerEvents, position, property, pseudoClass, px, relative, rem, resize, right, solid, top, transform, translateY, url, vertical, width, zero)
import Css.Global as Css
import Css.Transitions as Transition exposing (transition)
import DesignSystem.Colors as Colors
import DesignSystem.Spacing exposing (SpacingSize(..), spacing)
import DesignSystem.Typography as FontSize exposing (TypographyType(..), fontSize, typography)
import Html.Styled as Html exposing (Attribute, Html, aside, div, input, label, p, text, textarea)
import Html.Styled.Attributes as Attributes exposing (attribute, autofocus, checked, class, classList, css, for, id, maxlength, name, placeholder, rows, type_)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Maybe.Extra as Maybe
import Task
import Utils.Html exposing (nothing, screenReaderOnly, viewIf, viewIfLazy, viewMaybe)


type InputModel
    = InputModel { value : String, touchStatus : TouchStatus }


type CheckboxModel
    = CheckboxModel { value : Bool, touchStatus : TouchStatus }


type SearchInputModel
    = SearchInputModel { value : String }


searchValue : SearchInputModel -> String
searchValue (SearchInputModel model) =
    model.value


type TouchStatus
    = Pristine
    | Touched


type Required
    = Required
    | NotRequired


type Autofocus
    = Autofocus
    | NotAutofocus


type alias InputConfig =
    { name : String
    , label : String
    , autofocus : Autofocus
    , required : Required
    , maxSize : Maybe Int
    , validators : List Validator
    }


type alias CheckboxInputConfig =
    { name : String
    , required : Required
    , validators : List Validator
    }


type alias Validator =
    String -> Maybe String


staticValue : List (Html.Attribute msg) -> String -> Html msg
staticValue attributes content =
    div ([ class "staticValue" ] ++ attributes) [ text content ]


inputWithLabel : InputConfig -> InputModel -> (InputModel -> msg) -> List (Html.Attribute msg) -> Html msg
inputWithLabel config (InputModel model) toMsg attributes =
    let
        idName =
            nameToInputId config.name

        validationErrors =
            List.filterMap (\validator -> validator model.value) config.validators

        shouldShowErrors =
            model.touchStatus == Touched && (not <| List.isEmpty validationErrors)
    in
    div
        ([ class "inputWithLabel"
         , classList
            [ ( "inputWithLabel--empty", String.isEmpty model.value )
            , ( "inputWithLabel--error", shouldShowErrors )
            ]
         ]
            ++ attributes
        )
        [ input
            ([ id idName
             , class "input"
             , type_ "text"
             , name config.name
             , Attributes.value model.value
             , onInput (\newValue -> toMsg (InputModel { model | value = newValue }))
             , onBlur (toMsg <| InputModel { model | touchStatus = Touched })
             , autofocus (config.autofocus == Autofocus)
             ]
                ++ (Maybe.map (maxlength >> List.singleton) config.maxSize |> Maybe.withDefault [])
            )
            []
        , label [ for idName ]
            [ text config.label
            , viewIf (config.required == NotRequired) (text " (optional)")
            ]
        , viewIfLazy (model.touchStatus == Touched && (not <| List.isEmpty validationErrors)) (\() -> viewValidationErrors validationErrors)
        ]


searchInput : String -> SearchInputModel -> (SearchInputModel -> msg) -> List (Html.Attribute msg) -> Html msg
searchInput inputName (SearchInputModel model) toMsg attributes =
    let
        idName =
            nameToInputId inputName
    in
    div
        ([ class "searchInput" ] ++ attributes)
        [ input
            [ id idName
            , class "input"
            , type_ "text"
            , name inputName
            , Attributes.value model.value
            , onInput (\newValue -> toMsg (SearchInputModel { model | value = newValue }))
            , placeholder "Search"
            ]
            []
        ]


searchInputHeight : Px
searchInputHeight =
    px 32


textareaWithLabel : InputConfig -> InputModel -> (InputModel -> msg) -> List (Html.Attribute msg) -> Html msg
textareaWithLabel config (InputModel model) toMsg attributes =
    let
        idName =
            nameToInputId config.name

        validationErrors =
            List.filterMap (\validator -> validator model.value) config.validators

        shouldShowErrors =
            model.touchStatus == Touched && (not <| List.isEmpty validationErrors)
    in
    div
        ([ class "inputWithLabel"
         , classList
            [ ( "inputWithLabel--empty", String.isEmpty model.value )
            , ( "inputWithLabel--error", shouldShowErrors )
            ]
         ]
            ++ attributes
        )
        [ textarea
            ([ id idName
             , name config.name
             , class "input"
             , rows 3
             , onInput (\newValue -> toMsg (InputModel { model | value = newValue }))
             , onBlur (toMsg <| InputModel { model | touchStatus = Touched })
             , autofocus (config.autofocus == Autofocus)
             , Attributes.value model.value
             ]
                ++ (Maybe.map maxlength config.maxSize |> Maybe.toList)
            )
            []
        , label [ for idName ]
            [ text config.label
            , viewIf (config.required == NotRequired) (text " (optional)")
            ]
        , viewMaybe (viewTextareaCounter model.value) config.maxSize
        , viewIfLazy (model.touchStatus == Touched && (not <| List.isEmpty validationErrors)) (\() -> viewValidationErrors validationErrors)
        ]


checkbox : CheckboxInputConfig -> CheckboxModel -> (CheckboxModel -> msg) -> Html msg
checkbox checkboxInputConfig (CheckboxModel model) toMsg =
    div
        [ class "checkbox" ]
        [ input
            [ type_ "checkbox"
            , id (nameToInputId checkboxInputConfig.name)
            , name checkboxInputConfig.name
            , checked model.value
            , onClick (toMsg (CheckboxModel { model | value = not model.value, touchStatus = Touched }))
            ]
            []
        , label [ for (nameToInputId checkboxInputConfig.name) ] []
        ]


inputId : WhateverConfig a -> String
inputId inputConfig =
    nameToInputId inputConfig.name


type alias WhateverConfig a =
    { a
        | name : String
    }


touch : InputModel -> InputModel
touch (InputModel inputModel) =
    InputModel { inputModel | touchStatus = Touched }


emptyInput : InputModel
emptyInput =
    InputModel { value = "", touchStatus = Pristine }


checkboxModel : Bool -> CheckboxModel
checkboxModel initialValue =
    CheckboxModel { value = initialValue, touchStatus = Pristine }


isChecked : CheckboxModel -> Bool
isChecked (CheckboxModel model) =
    model.value


emptySearchInput : SearchInputModel
emptySearchInput =
    SearchInputModel { value = "" }


prefilledSearchInput : String -> SearchInputModel
prefilledSearchInput initialValue =
    SearchInputModel { value = initialValue }


prefilledInput : String -> InputModel
prefilledInput initialValue =
    InputModel { value = initialValue, touchStatus = Pristine }


value : InputModel -> String
value (InputModel inputModel) =
    inputModel.value


focus : msg -> InputConfig -> Cmd msg
focus msg inputConfig =
    Dom.focus (nameToInputId inputConfig.name)
        |> Task.attempt (always msg)


haveErrors : List ( InputConfig, InputModel ) -> Bool
haveErrors fields =
    List.any (\( config, model ) -> hasErrors config model) fields


hasErrors : InputConfig -> InputModel -> Bool
hasErrors config (InputModel model) =
    List.any (\validator -> validator model.value |> Maybe.isJust) config.validators


viewTextareaCounter : String -> Int -> Html msg
viewTextareaCounter value_ limit =
    let
        counterLabel =
            String.fromInt (String.length value_) ++ "/" ++ String.fromInt limit
    in
    div [ css [ position absolute, right (px 12), bottom (px 32) ] ]
        [ typography Paragraph aside [] counterLabel
        ]


viewValidationErrors : List String -> Html msg
viewValidationErrors errors =
    List.head errors
        |> Maybe.map
            (\firstError ->
                p [ class "validationErrors", attribute "aria-live" "assertive" ]
                    [ screenReaderOnly "Error:"
                    , text firstError
                    ]
            )
        |> Maybe.withDefault nothing


nameToInputId : String -> String
nameToInputId name =
    "input-" ++ name


radioId : String -> Int -> String
radioId inputName position =
    "radio-" ++ inputName ++ "-" ++ String.fromInt position



---- Styles ----


styles : List Css.Snippet
styles =
    [ Css.class "inputWithLabel"
        [ position relative
        , paddingBottom (px 20)
        , Css.children
            [ Css.label
                [ position absolute
                , top zero
                , left (px 9)
                , transform (translateY (pct -50))
                , fontSize FontSize.XS
                , padding2 zero (px 3)
                , color Colors.typographyMedium
                , property "transition" "all 0.1s"
                , backgroundColor Colors.white
                ]
            , Css.class "input"
                (commonInputStyles
                    ++ [ border3 (px 1) solid Colors.fieldBorder
                       , Css.hover [ borderColor Colors.fieldBorderHover ]
                       , resize vertical
                       ]
                )
            ]
        , Css.withClass "inputWithLabel--empty"
            [ Css.children
                [ Css.class "input"
                    [ pseudoClass "not(:focus)"
                        [ Css.adjacentSiblings
                            [ Css.label
                                [ top (px 13)
                                , left (px 12)
                                , height (rem 0.9)
                                , fontSize FontSize.S
                                , transform (translateY zero)
                                , pointerEvents none
                                , backgroundImage none
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , Css.withClass "inputWithLabel--error"
            [ Css.children
                [ Css.class "input"
                    [ borderColor Colors.fieldBorderError
                    ]
                ]
            ]
        ]
    , Css.class "staticValue" (commonInputStyles ++ [ backgroundColor Colors.typographyHigh, marginBottom (px 20) ])
    , Css.class "validationErrors"
        [ color Colors.fieldError
        , position absolute
        , padding2 zero (px 12)
        , marginTop (spacing XXS)
        , fontSize FontSize.XS
        ]
    , Css.class "searchInput"
        [ position relative
        , Css.children
            [ Css.class "input"
                [ height searchInputHeight
                , width (pct 100)
                , borderRadius (px 3)
                , padding (px 12)
                , backgroundColor Colors.fieldBackground
                , color Colors.fieldInputText
                , Css.focus [ outlineColor Colors.fieldFocus ]
                , Css.hover
                    [ color Colors.fieldInputTextHoverActive
                    ]
                , border3 (px 1) solid Colors.fieldBorder
                , Css.hover [ borderColor Colors.fieldBorderHover ]
                , padding4 (px 6) (px 32) (px 6) (px 12)
                , fontSize FontSize.M
                ]
            ]
        , Css.after
            [ property "content" "''"
            , display block
            , width (px 16)
            , height (px 16)

            --, backgroundImage (url imagePaths.magnifyingGlass)
            , position absolute
            , backgroundSize contain
            , right (px 12)
            , top (px 8)
            , pointerEvents none
            ]
        ]
    , Css.class "checkbox"
        [ paddingRight (px 3)
        , position relative
        , Css.children
            [ Css.input
                [ opacity (int 0)
                , position absolute
                , Css.focus
                    [ Css.adjacentSiblings
                        [ Css.label
                            [ boxShadow5 (px 0) (px 0) (px 0) (px 3) Colors.fieldFocus
                            ]
                        ]
                    ]
                , Css.checked
                    [ Css.adjacentSiblings
                        [ Css.label
                            [ backgroundColor Colors.checkedRadioInputBackground
                            , border3 (px 2) solid Colors.checkedRadioInputBorder
                            , Css.after [ left (px 20) ]
                            ]
                        ]
                    ]
                , Css.adjacentSiblings
                    [ Css.label
                        [ display block
                        , width (px 50)
                        , height (px 30)
                        , backgroundColor Colors.radioInputBackgroundColor
                        , border3 (px 2) solid Colors.radioInputBorder
                        , borderRadius (px 15)
                        , position relative
                        , cursor pointer
                        , transition [ Transition.backgroundColor 200 ]
                        , Css.after
                            [ property "content" "''"
                            , position absolute
                            , top zero
                            , left (px 0)
                            , width (px 26)
                            , height (px 26)
                            , border3 (px 1) solid Colors.fieldBorder
                            , backgroundColor Colors.white
                            , display block
                            , borderRadius (pct 100)
                            , boxSizing borderBox
                            , transition [ Transition.left 100 ]
                            , boxShadow4 zero (px 1) (px 1) Colors.fieldBorder
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]


commonInputStyles : List Style
commonInputStyles =
    [ minHeight (px 40)
    , width (pct 100)
    , borderRadius (px 3)
    , padding (px 12)
    , backgroundColor Colors.fieldBackground
    , color Colors.fieldInputText
    , Css.focus [ outlineColor Colors.fieldFocus ]
    , Css.hover
        [ color Colors.fieldInputTextHoverActive
        ]
    ]
