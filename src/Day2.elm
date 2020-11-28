module Day2 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (absolute, backgroundColor, block, borderRadius, calc, center, display, displayFlex, flexWrap, height, justifyContent, left, margin, minWidth, minus, paddingTop, pct, position, px, relative, rgb, right, textAlign, textTransform, top, uppercase, width, wrap)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop, padding2)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, form, h1, input, p, text)
import Html.Styled.Attributes exposing (class, css, type_, value)
import Html.Styled.Events exposing (onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)


type alias Enigma =
    { clue : String, answers : Set String }


type Model
    = InProgress { tries : Int, current : Enigma, remaining : List Enigma, fieldValue : String }
    | Done Int


init : Model
init =
    InProgress { tries = 0, current = firstEnigma, remaining = otherEnigmas, fieldValue = "" }


firstEnigma : Enigma
firstEnigma =
    { clue = "Grande Maman Pâques"
    , answers = Set.fromList [ "Petit papa Noël" ]
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { clue = "le grillon et le termite"
      , answers = Set.fromList [ "la cigale et la fourmi", "la cigale et la fourmie" ]
      }
    , { clue = "combat deux"
      , answers = Set.fromList [ "lutin", "un lutin" ]
      }
    , { clue = "Kebab Questionnaire"
      , answers = Set.fromList [ "Burger Quiz", "Burger Quizz" ]
      }
    , { clue = "charrie lait"
      , answers = Set.fromList [ "traineau", "un traineau" ]
      }
    ]


type Msg
    = Try String
    | FieldChanged String


normalize : String -> String
normalize string =
    String.replace " " "" string
        |> removeDiacritics
        |> String.toLower


update : Model -> Msg -> Model
update state msg =
    case ( state, msg ) of
        ( InProgress model, Try answer ) ->
            if Set.member (normalize answer) (Set.map normalize model.current.answers) then
                case model.remaining of
                    firstRemaining :: others ->
                        InProgress
                            { model
                                | tries = model.tries + 1
                                , current = firstRemaining
                                , remaining = others
                                , fieldValue = ""
                            }

                    [] ->
                        Done (model.tries + 1)

            else
                InProgress { model | tries = model.tries + 1, fieldValue = "" }

        ( InProgress model, FieldChanged value ) ->
            InProgress { model | fieldValue = value }

        ( Done _, _ ) ->
            state


getScore : Model -> Int
getScore state =
    case state of
        InProgress model ->
            (5 - List.length model.remaining - 1) * 5 - model.tries

        Done tries ->
            25 - tries


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay <= 2 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day2" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 2 | Contrario"
            , case state of
                InProgress model ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Retrouvez le mot ou l'expression initiale en remplaçant les mots par leur synonyme, leur contraire ou un mot similaire dans l'indice."
                        , typography Paragraph p [ css [ textAlign center, marginTop Spacing.S ] ] "Exemple 1 : Les saisons anciennes => Les temps modernes"
                        , typography Paragraph p [ css [ textAlign center, marginBottom Spacing.S ] ] "Exemple 2 : Un étain au poulain => Un fer à cheval"
                        , typography Instructions p [ css [ textAlign center ] ] "(Les accents et majuscules ne sont pas importants.)"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.M, marginTop Spacing.S, textTransform uppercase ] ] ("Indice : " ++ model.current.clue)
                        , form [ onSubmit (Try model.fieldValue), css [ textAlign center ] ]
                            [ input [ type_ "text", value model.fieldValue, onInput FieldChanged ] []
                            , button [ type_ "submit" ] [ text "Valider" ]
                            ]
                        ]

                Done _ ->
                    typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] ("Défi terminé ! Votre score : " ++ String.fromInt (getScore state))
            ]


isDone : Model -> Bool
isDone model =
    case model of
        Done _ ->
            True

        InProgress _ ->
            False


saveState : Model -> Encode.Value
saveState state =
    case state of
        InProgress model ->
            Encode.object
                [ ( "state", Encode.string "in-progress" )
                , ( "tries", Encode.int model.tries )
                , ( "remaining", Encode.int (List.length model.remaining + 1) )
                ]

        Done tries ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "tries", Encode.int tries )
                ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "in-progress" ->
                        Decode.map2
                            (\tries remaining ->
                                if remaining == 5 then
                                    InProgress { tries = tries, current = firstEnigma, remaining = otherEnigmas, fieldValue = "" }
                                        |> Decode.succeed

                                else
                                    let
                                        remainingEnigmas =
                                            List.drop (5 - remaining - 1) otherEnigmas
                                    in
                                    case remainingEnigmas of
                                        first :: others ->
                                            InProgress { tries = tries, current = first, remaining = others, fieldValue = "" }
                                                |> Decode.succeed

                                        [] ->
                                            Decode.fail "Invalid state"
                            )
                            (Decode.field "tries" Decode.int)
                            (Decode.field "remaining" Decode.int)
                            |> Decode.andThen identity

                    "done" ->
                        Decode.map Done (Decode.field "tries" Decode.int)

                    stateValue ->
                        Decode.fail ("Unknown state value: " ++ stateValue)
            )


styles : List Snippet
styles =
    [ Css.class "grid"
        [ displayFlex
        , flexWrap wrap
        , justifyContent center
        ]
    , Css.class "card"
        [ borderRadius (px 5)
        , backgroundColor (rgb 236 200 87)
        , width (calc (pct 20) minus (px 10))
        , minWidth (px 120)
        , height (px 80)
        , margin (px 5)
        , paddingTop (px 10)
        , position relative
        , Css.children
            [ Css.class "word"
                [ position absolute
                , display block
                , left (px 10)
                , right (px 10)
                , top (px 40)
                , backgroundColor (rgb 246 221 140)
                , padding2 Spacing.XXS Spacing.XS
                , borderRadius (px 5)
                , textTransform uppercase
                ]
            ]
        , Css.withClass "card--correct"
            [ backgroundColor (rgb 91 172 74)
            , Css.children
                [ Css.class "word"
                    [ backgroundColor (rgb 217 255 212)
                    ]
                ]
            ]
        , Css.withClass "card--wrong"
            [ backgroundColor (rgb 212 0 0)
            , Css.children
                [ Css.class "word"
                    [ backgroundColor (rgb 255 169 169)
                    ]
                ]
            ]
        ]
    ]
