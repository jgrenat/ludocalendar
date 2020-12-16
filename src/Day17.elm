module Day17 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (backgroundColor, borderWidth, center, color, displayFlex, flexWrap, height, justifyContent, maxWidth, pct, px, rgb, right, textAlign, transparent, wrap, zero)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, a, button, div, form, h1, img, input, p, span, text)
import Html.Styled.Attributes exposing (alt, class, css, href, src, target, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Nonempty as Nonempty exposing (Nonempty)
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)
import Utils.Html exposing (viewIf)


type alias Enigma =
    { images : List String, answers : Nonempty String, clue : String }


type Model
    = InProgress { tries : Int, enigmas : Nonempty Enigma, isClueShown : Bool, fieldValue : String }
    | Done Int


init : Model
init =
    InProgress { tries = 0, enigmas = Nonempty.Nonempty firstEnigma otherEnigmas, isClueShown = False, fieldValue = "" }


firstEnigma : Enigma
firstEnigma =
    { images = [ ImagePath.toString images.day17.day1711, ImagePath.toString images.day17.day1712, ImagePath.toString images.day17.day1713 ]
    , answers = Nonempty.Nonempty "Sudoku" [ "le sudoku", "un sudoku", "une grille de sudoku" ]
    , clue = "Jeu japonais"
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { images = [ ImagePath.toString images.day17.day1731, ImagePath.toString images.day17.day1732, ImagePath.toString images.day17.day1733, ImagePath.toString images.day17.day1734, ImagePath.toString images.day17.day1735, ImagePath.toString images.day17.day1736, ImagePath.toString images.day17.day1737 ]
      , answers = Nonempty.Nonempty "Prison Break" [ "la série Prison Break" ]
      , clue = "La grande évasion"
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
                                | tries = model.tries + 1
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
            InProgress { model | isClueShown = True, tries = model.tries + 2 }

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
            max 0 (finished * 11 - model.tries)

        Done tries ->
            max 0 (22 - tries)


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate + 1
    in
    if maxDay < 17 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 17 | Concept"
            , case state of
                InProgress model ->
                    let
                        enigma =
                            Nonempty.head model.enigmas
                    in
                    div []
                        [ p [ css [ textAlign center ] ]
                            [ typography Instructions span [] "Retrouvez le mot ou l'expression grâce aux indices sur le plateau de jeu. "
                            , typography Instructions a [ href "https://www.regledujeu.fr/concept/", target "_blank" ] "Voir les règles de Concept"
                            ]
                        , p [ css [ marginTop Spacing.M, textAlign center ] ]
                            [ typography Instructions a [ href (ImagePath.toString images.day17.rules), target "_blank" ] "🔎 Cliquez ici pour voir la signification des symboles"
                            ]
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , div [ class "images" ] (List.map (\image -> img [ src image, alt "image de l'énigme", class "image" ] []) enigma.images)
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.S, marginBottom Spacing.XS ] ] "(Les accents et majuscules ne sont pas importants.)"
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
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 17 ]
                        , p [] [ twitterLink 17 ]
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
                                if remaining == 2 then
                                    InProgress { tries = tries, enigmas = Nonempty.Nonempty firstEnigma otherEnigmas, fieldValue = "", isClueShown = isClueShown }
                                        |> Decode.succeed

                                else
                                    let
                                        remainingEnigmas =
                                            List.drop (2 - remaining - 1) otherEnigmas
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
    [ Css.class "images"
        [ displayFlex
        , flexWrap wrap
        , justifyContent center
        , marginBottom Spacing.M
        ]
    , Css.class "image"
        [ height (px 150)
        , maxWidth (pct 100)
        ]
    , Css.class "clue-button"
        [ backgroundColor transparent
        , borderWidth zero
        , color (rgb 0 0 0)
        , Css.hover [ backgroundColor (rgb 230 230 230) ]
        , Css.focus [ backgroundColor (rgb 210 210 210) ]
        ]
    ]
