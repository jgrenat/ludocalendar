module Day15 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (auto, backgroundColor, block, bold, borderWidth, center, color, display, fontWeight, height, margin, maxHeight, maxWidth, pct, px, rgb, right, textAlign, transparent, vh, zero)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, form, h1, img, input, p, span, text)
import Html.Styled.Attributes exposing (alt, class, css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)
import Utils.Html exposing (viewIf)


type alias Enigma =
    { image : String, answers : Nonempty String, theme : String, clue : String }


type Model
    = InProgress { tries : Int, current : Enigma, remaining : List Enigma, fieldValue : String, showClue : Bool }
    | Done Int


init : Model
init =
    InProgress { tries = 0, current = firstEnigma, remaining = otherEnigmas, fieldValue = "", showClue = False }


firstEnigma : Enigma
firstEnigma =
    { image = ImagePath.toString images.day15.day157
    , answers = Nonempty "Une glace" [ "glace", "la glace", "sorbet", "cone", "glaces", "des glaces" ]
    , theme = "Miam"
    , clue = "Sucrerie froide"
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { image = ImagePath.toString images.day15.day153
      , answers = Nonempty "Une sirène" [ "sirène", "la sirène", "sirènes", "des sirènes", "les sirènes" ]
      , theme = "Mythes et légendes"
      , clue = "Animal fantastique marin"
      }
    , { image = ImagePath.toString images.day15.day154
      , answers = Nonempty "La Belle et la Bête" [ "belle et la bête" ]
      , theme = "Film"
      , clue = "Disney"
      }
    , { image = ImagePath.toString images.day15.day158
      , answers = Nonempty "Un ascenseur" [ "ascenseur" ]
      , theme = "Objet"
      , clue = "Dans un immeuble"
      }
    , { image = ImagePath.toString images.day15.day156
      , answers = Nonempty "Le saut à skis" [ "Le saut à ski", "saut à skis", "saut à ski", "saut de ski", "saut de skis", "saut en skis", "saut en ski" ]
      , theme = "Sport"
      , clue = "Sport d'hiver"
      }
    , { image = ImagePath.toString images.day15.day151
      , answers = Nonempty "Un hypnotiseur" [ "hypnotiseur", "hypnose", "l'hypnose", "hypnotiseuse" ]
      , theme = "Drôle de métier"
      , clue = "Freud en était également un"
      }
    , { image = ImagePath.toString images.day15.day152
      , answers = Nonempty "Une sarbacane" [ "sarbacane", "la sarbacane", "sarbacanes", "des sarbacanes", "les sarbacanes", "sarbaccanes", "sarbaccane", "sarbacanne", "sarbacannes" ]
      , theme = "Objet"
      , clue = "Arme de jet"
      }
    ]


type Msg
    = Try String
    | ShowClue
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
            if Nonempty.member (normalize answer) (Nonempty.map normalize model.current.answers) then
                case model.remaining of
                    firstRemaining :: others ->
                        InProgress
                            { model
                                | tries = model.tries + 1
                                , current = firstRemaining
                                , remaining = others
                                , showClue = False
                                , fieldValue = ""
                            }

                    [] ->
                        Done (model.tries + 1)

            else
                InProgress { model | tries = model.tries + 1, fieldValue = "" }

        ( InProgress model, FieldChanged value ) ->
            InProgress { model | fieldValue = value }

        ( InProgress model, ShowClue ) ->
            InProgress { model | showClue = True }

        ( Done _, _ ) ->
            state


getScore : Model -> Int
getScore state =
    case state of
        InProgress model ->
            let
                finished =
                    7 - List.length model.remaining - 1
            in
            finished * 4 - max 0 model.tries

        Done tries ->
            28 - tries


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay < 15 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day2" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 15 | Imagine"
            , case state of
                InProgress model ->
                    div []
                        [ p [ css [ textAlign center ] ]
                            [ typography Instructions span [] "Retrouvez le mot ou l'expression grâce à l'agencement de cartes transparentes. "
                            ]
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.S ] ] "(Les accents et majuscules ne sont pas importants.)"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , Keyed.node "div"
                            []
                            [ ( String.fromInt (List.length model.remaining), img [ src model.current.image, alt "image de l'énigme", class "image" ] [] )
                            ]
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Thème : " ++ model.current.theme)
                        , div [ css [ textAlign center, marginBottom Spacing.M ] ]
                            [ button [ class "clue-button", onClick ShowClue ] [ text "Demander un indice (vous ne perdrez pas de point)" ]
                            ]
                            |> viewIf (not model.showClue)
                        , typography Instructions p [ css [ textAlign center, marginBottom Spacing.M ] ] ("Indice : " ++ model.current.clue)
                            |> viewIf model.showClue
                        , form [ onSubmit (Try model.fieldValue), css [ textAlign center ] ]
                            [ input [ type_ "text", value model.fieldValue, onInput FieldChanged ] []
                            , button [ type_ "submit" ] [ text "Valider" ]
                            ]
                        ]

                Done _ ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] ("Défi terminé ! Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 15 ]
                        , p [] [ twitterLink 15 ]
                        ]
            , typography Paragraph p [ css [ marginTop Spacing.L, fontWeight bold, textAlign center ] ] "Un énorme merci à Patrick qui a réalisé et pris en photo les énigmes d'aujourd'hui ! 🎉"
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
                                if remaining == 7 then
                                    InProgress { tries = tries, current = firstEnigma, remaining = otherEnigmas, fieldValue = "", showClue = False }
                                        |> Decode.succeed

                                else
                                    let
                                        remainingEnigmas =
                                            List.drop (7 - remaining - 1) otherEnigmas
                                    in
                                    case remainingEnigmas of
                                        first :: others ->
                                            InProgress { tries = tries, current = first, remaining = others, fieldValue = "", showClue = False }
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
    [ Css.class "image"
        [ maxHeight (vh 65)
        , maxWidth (pct 100)
        , display block
        , margin auto
        ]
    , Css.class "clue-button"
        [ backgroundColor transparent
        , borderWidth zero
        , color (rgb 0 0 0)
        , Css.hover [ backgroundColor (rgb 230 230 230) ]
        , Css.focus [ backgroundColor (rgb 210 210 210) ]
        ]
    ]
