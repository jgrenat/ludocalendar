module Day19 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (backgroundColor, borderWidth, center, color, rgb, right, textAlign, textTransform, transparent, uppercase, zero)
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
import List.Nonempty as Nonempty exposing (Nonempty)
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)
import Utils.Html exposing (viewIf)


type alias Enigma =
    { question : String, answers : Nonempty String, clue : String }


type Model
    = InProgress { tries : Int, enigmas : Nonempty Enigma, isClueShown : Bool, fieldValue : String }
    | Done Int


init : Model
init =
    InProgress { tries = 0, enigmas = Nonempty.Nonempty firstEnigma otherEnigmas, isClueShown = False, fieldValue = "" }


firstEnigma : Enigma
firstEnigma =
    { question = "Soulever une tartine"
    , answers = Nonempty.Nonempty "Porter un toast" [ "porter toast", "porter le toast" ]
    , clue = "Discours et félicitations"
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { question = "Un champion-à-poitrine"
      , answers = Nonempty.Nonempty "Un assassin" [ "assassin", "l'assassin", "assassins", "des assassins", "les assassins" ]
      , clue = "Meurtrier"
      }
    , { question = "Une occase du tonnerre"
      , answers = Nonempty.Nonempty "Un coup de foudre" [ "Le coup de foudre", "coup de foudre" ]
      , clue = "Amour instantané"
      }
    , { question = "Détesta Holmes"
      , answers = Nonempty.Nonempty "Emma Watson" [ "Ema Watson" ]
      , clue = "Sorcière bien aimée"
      }
    , { question = "Tuba-à-froid"
      , answers = Nonempty.Nonempty "Palmashow" [ "Le palmashow", "palm-a-show", "le palm-a-show", "palme-a-show", "le palme-a-show" ]
      , clue = "Sketchs comiques"
      }
    ]


type Msg
    = Try String
    | FieldChanged String
    | ShowClue


normalize : String -> String
normalize string =
    String.replace " " "" string
        |> removeDiacritics
        |> String.toLower


update : Model -> Msg -> Model
update state msg =
    case ( state, msg ) of
        ( InProgress model, Try answer ) ->
            let
                enigma =
                    Nonempty.head model.enigmas

                isCorrect =
                    Nonempty.member (normalize answer) (Nonempty.map normalize enigma.answers)
            in
            if isCorrect then
                case Nonempty.tail model.enigmas of
                    firstRemaining :: others ->
                        InProgress
                            { model
                                | tries = model.tries + 2
                                , enigmas = Nonempty.Nonempty firstRemaining others
                                , fieldValue = ""
                                , isClueShown = False
                            }

                    [] ->
                        Done (model.tries + 1)

            else
                InProgress { model | tries = model.tries + 1, fieldValue = "" }

        ( InProgress model, FieldChanged value ) ->
            InProgress { model | fieldValue = value }

        ( InProgress model, ShowClue ) ->
            InProgress { model | isClueShown = True, tries = model.tries + 1 }

        ( Done _, _ ) ->
            state


getScore : Model -> Int
getScore state =
    case state of
        InProgress model ->
            let
                finished =
                    (List.length otherEnigmas + 1) - Nonempty.length model.enigmas
            in
            max 0 (finished * 6 - model.tries)

        Done tries ->
            max 0 (30 - tries)


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate + 1
    in
    if maxDay < 19 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 19 | Contrario"
            , case state of
                InProgress model ->
                    let
                        enigma =
                            Nonempty.head model.enigmas
                    in
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Retrouvez le mot ou l'expression initiale en remplaçant les mots par leur synonyme, leur contraire ou un mot similaire dans l'indice."
                        , typography Paragraph p [ css [ textAlign center, marginTop Spacing.S ] ] "Exemple 1 : Les saisons anciennes => Les temps modernes"
                        , typography Paragraph p [ css [ textAlign center, marginBottom Spacing.S ] ] "Exemple 2 : Un étain au poulain => Un fer à cheval"
                        , typography Instructions p [ css [ textAlign center ] ] "(Les accents et majuscules ne sont pas importants.)"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.M, marginTop Spacing.S, textTransform uppercase ] ] enigma.question
                        , form [ onSubmit (Try model.fieldValue), css [ textAlign center ] ]
                            [ input [ type_ "text", value model.fieldValue, onInput FieldChanged ] []
                            , button [ type_ "submit" ] [ text "Valider" ]
                            ]
                        , div [ css [ textAlign center, marginTop Spacing.M ] ]
                            [ button [ class "clue-button", onClick ShowClue ] [ text "😫 J'ai besoin d'un indice ! (-2 points)" ]
                            ]
                            |> viewIf (not model.isClueShown)
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] ("Indice : " ++ enigma.clue)
                            |> viewIf model.isClueShown
                        ]

                Done _ ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] ("Défi terminé ! Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 19 ]
                        , p [] [ twitterLink 19 ]
                        ]
            , homeLink
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
                , ( "remaining", Encode.int (Nonempty.length model.enigmas) )
                , ( "is-clue-shown", Encode.bool model.isClueShown )
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
                        Decode.map3
                            (\tries remaining isClueShown ->
                                if remaining == 5 then
                                    InProgress { tries = tries, enigmas = Nonempty.Nonempty firstEnigma otherEnigmas, fieldValue = "", isClueShown = isClueShown }
                                        |> Decode.succeed

                                else
                                    let
                                        remainingEnigmas =
                                            List.drop (5 - remaining - 1) otherEnigmas
                                    in
                                    case remainingEnigmas of
                                        first :: others ->
                                            InProgress { tries = tries, enigmas = Nonempty.Nonempty first others, fieldValue = "", isClueShown = isClueShown }
                                                |> Decode.succeed

                                        [] ->
                                            Decode.fail "Invalid state"
                            )
                            (Decode.field "tries" Decode.int)
                            (Decode.field "remaining" Decode.int)
                            (Decode.field "is-clue-shown" Decode.bool)
                            |> Decode.andThen identity

                    "done" ->
                        Decode.map Done (Decode.field "tries" Decode.int)

                    stateValue ->
                        Decode.fail ("Unknown state value: " ++ stateValue)
            )


styles : List Snippet
styles =
    [ Css.class "clue-button"
        [ backgroundColor transparent
        , borderWidth zero
        , color (rgb 0 0 0)
        , Css.hover [ backgroundColor (rgb 230 230 230) ]
        , Css.focus [ backgroundColor (rgb 210 210 210) ]
        ]
    ]
