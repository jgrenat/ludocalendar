module Day2 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (center, right, textAlign, textTransform, uppercase)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, form, h1, input, p, text)
import Html.Styled.Attributes exposing (class, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)
import Utils.Html exposing (viewMaybe)


type alias Enigma =
    { clue : String, answers : Set String }


type Model
    = InProgress { giveUp : Int, tries : Int, current : Enigma, remaining : List Enigma, fieldValue : String, showAnswer : Maybe String }
    | Done Int Int (Maybe String)


init : Model
init =
    InProgress { giveUp = 0, tries = 0, current = firstEnigma, remaining = otherEnigmas, fieldValue = "", showAnswer = Nothing }


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
    , { clue = "bagarre zéro"
      , answers = Set.fromList [ "lutin", "lutin" ]
      }
    , { clue = "Kebab Questionnaire"
      , answers = Set.fromList [ "Burger Quiz", "Burger Quizz" ]
      }
    , { clue = "L'almanach de l'Après"
      , answers = Set.fromList [ "Le calendrier de l'Avent", "calendrier de l'Avent", "Le calendrier de l'Avant", "calendrier de l'Avant", "Un calendrier de l'Avent", "Un calendrier de l'Avant" ]
      }
    ]


type Msg
    = Try String
    | FieldChanged String
    | GiveUp


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
                                , showAnswer = Nothing
                            }

                    [] ->
                        Done model.giveUp (model.tries + 1) Nothing

            else
                InProgress { model | tries = model.tries + 1, fieldValue = "", showAnswer = Nothing }

        ( InProgress model, FieldChanged value ) ->
            InProgress { model | fieldValue = value }

        ( InProgress model, GiveUp ) ->
            case model.remaining of
                firstRemaining :: others ->
                    InProgress
                        { model
                            | current = firstRemaining
                            , remaining = others
                            , fieldValue = ""
                            , showAnswer = Set.toList model.current.answers |> List.head
                            , giveUp = model.giveUp + 1
                        }

                [] ->
                    Done (model.giveUp + 1) model.tries (Set.toList model.current.answers |> List.head)

        ( Done _ _ _, _ ) ->
            state


getScore : Model -> Int
getScore state =
    case state of
        InProgress model ->
            (5 - List.length model.remaining - model.giveUp - 1) * 5 - model.tries

        Done giveUp tries _ ->
            (25 - 5 * giveUp) - tries


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay < 2 then
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
                        , viewMaybe viewLastAnswer model.showAnswer
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.M, marginTop Spacing.S, textTransform uppercase ] ] ("Indice : " ++ model.current.clue)
                        , form [ onSubmit (Try model.fieldValue), css [ textAlign center ] ]
                            [ input [ type_ "text", value model.fieldValue, onInput FieldChanged ] []
                            , button [ type_ "submit" ] [ text "Valider" ]
                            , p [ css [ marginTop Spacing.S ] ]
                                [ button [ type_ "button", class "button--secondary", onClick GiveUp ] [ text "Passer cette énigme" ]
                                ]
                            ]
                        ]

                Done _ _ showAnswer ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ viewMaybe viewLastAnswer showAnswer
                        , typography HeroText p [] ("Défi terminé ! Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 2 ]
                        , p [] [ twitterLink 2 ]
                        ]
            , homeLink
            ]


viewLastAnswer : String -> Html msg
viewLastAnswer answer =
    typography Instructions p [ css [ textAlign center, marginTop Spacing.M, marginBottom Spacing.M ] ] ("La réponse était : " ++ answer)


isDone : Model -> Bool
isDone model =
    case model of
        Done _ _ _ ->
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
                , ( "giveUp", Encode.int model.giveUp )
                ]

        Done giveUp tries _ ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "tries", Encode.int tries )
                , ( "giveUp", Encode.int giveUp )
                ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "in-progress" ->
                        Decode.map3
                            (\giveUp tries remaining ->
                                if remaining == 5 then
                                    InProgress { tries = tries, current = firstEnigma, remaining = otherEnigmas, fieldValue = "", showAnswer = Nothing, giveUp = giveUp }
                                        |> Decode.succeed

                                else
                                    let
                                        remainingEnigmas =
                                            List.drop (5 - remaining - 1) otherEnigmas
                                    in
                                    case remainingEnigmas of
                                        first :: others ->
                                            InProgress { tries = tries, current = first, remaining = others, fieldValue = "", showAnswer = Nothing, giveUp = giveUp }
                                                |> Decode.succeed

                                        [] ->
                                            Decode.fail "Invalid state"
                            )
                            (Decode.field "giveUp" Decode.int)
                            (Decode.field "tries" Decode.int)
                            (Decode.field "remaining" Decode.int)
                            |> Decode.andThen identity

                    "done" ->
                        Decode.map3 Done
                            (Decode.field "giveUp" Decode.int)
                            (Decode.field "tries" Decode.int)
                            (Decode.succeed Nothing)

                    stateValue ->
                        Decode.fail ("Unknown state value: " ++ stateValue)
            )


styles : List Snippet
styles =
    []
