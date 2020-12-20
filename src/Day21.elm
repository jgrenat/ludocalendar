module Day21 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (backgroundColor, bold, borderColor, borderWidth, center, color, displayFlex, flexWrap, fontWeight, height, justifyContent, maxWidth, pct, px, rgb, right, textAlign, transparent, width, wrap, zero)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Colors as Colors
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop, padding)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, h1, img, p, text)
import Html.Styled.Attributes exposing (alt, class, css, src, type_)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Nonempty as Nonempty exposing (Nonempty)
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)


type alias Enigma =
    { image : String, answer : Item }


type Model
    = ReadingInstructions
    | InProgress EnigmaState
    | Done Int


type EnigmaState
    = Step { score : Int, enigmas : Nonempty Enigma }
    | StepResult Bool { score : Int, enigmas : Nonempty Enigma }


type Item
    = Ghost
    | Book
    | Mouse
    | Bottle
    | Chair


init : Model
init =
    ReadingInstructions


firstEnigma : Enigma
firstEnigma =
    { image = ImagePath.toString images.day21.enigma1
    , answer = Chair
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { image = ImagePath.toString images.day21.enigma2
      , answer = Mouse
      }
    , { image = ImagePath.toString images.day21.enigma3
      , answer = Chair
      }
    , { image = ImagePath.toString images.day21.enigma10
      , answer = Ghost
      }
    , { image = ImagePath.toString images.day21.enigma5
      , answer = Chair
      }
    , { image = ImagePath.toString images.day21.enigma6
      , answer = Mouse
      }
    , { image = ImagePath.toString images.day21.enigma9
      , answer = Book
      }
    , { image = ImagePath.toString images.day21.enigma4
      , answer = Mouse
      }
    , { image = ImagePath.toString images.day21.enigma7
      , answer = Ghost
      }
    , { image = ImagePath.toString images.day21.enigma8
      , answer = Mouse
      }
    ]


type Msg
    = Start
    | Try Item
    | GoToNextEnigma
    | TimeOver


normalize : String -> String
normalize string =
    String.replace " " "" string
        |> removeDiacritics
        |> String.toLower


update : Model -> Msg -> Model
update modelState msg =
    case ( modelState, msg ) of
        ( ReadingInstructions, Start ) ->
            Step { enigmas = Nonempty.Nonempty firstEnigma otherEnigmas, score = 0 }
                |> InProgress

        ( _, Start ) ->
            modelState

        ( InProgress (Step model), Try item ) ->
            if item == (Nonempty.head model.enigmas).answer then
                StepResult True { model | score = model.score + 2 }
                    |> InProgress

            else
                StepResult False { model | score = model.score }
                    |> InProgress

        ( _, Try _ ) ->
            modelState

        ( InProgress (Step model), TimeOver ) ->
            StepResult False { model | score = model.score }
                |> InProgress

        ( _, TimeOver ) ->
            modelState

        ( InProgress (StepResult _ model), GoToNextEnigma ) ->
            case Nonempty.tail model.enigmas of
                first :: others ->
                    Step { model | enigmas = Nonempty.Nonempty first others } |> InProgress

                [] ->
                    Done model.score

        ( _, GoToNextEnigma ) ->
            modelState


getScore : Model -> Int
getScore state =
    case state of
        InProgress (Step model) ->
            model.score

        InProgress (StepResult _ model) ->
            model.score

        ReadingInstructions ->
            0

        Done score ->
            score


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate + 1
    in
    if maxDay < 21 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 21 | Bazar Bizarre"
            , case state of
                ReadingInstructions ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Observez bien les 5 objets ci-dessous... Des cartes vont apparaître, si vous retrouvez l'un d'eux sur une carte avec la bonne forme et la bonne couleur, cliquez sur l'objet correspondant ! Si aucun ne correspond, cliquez sur l'objet qui n'a aucun lien avec la carte : ni la forme, ni la couleur !"
                        , p [ css [ textAlign center, marginTop Spacing.L ] ]
                            [ button [ type_ "button", onClick Start ] [ text "Commencer !" ]
                            ]
                        ]

                InProgress (Step model) ->
                    let
                        enigma =
                            Nonempty.head model.enigmas
                    in
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Cliquez sur le bon objet : celui identique à l'image s'il existe, sinon celui qui n'a aucune caractéristique commune (couleur et forme)."
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ textAlign center ] ] [ img [ src enigma.image, alt "Image de l'énigme", class "enigma-image" ] [] ]
                        , div [ class "items" ]
                            [ button [ type_ "button", onClick (Try Ghost), class "answer-image" ] [ img [ src (ImagePath.toString images.day21.ghost) ] [] ]
                            , button [ type_ "button", onClick (Try Book), class "answer-image" ] [ img [ src (ImagePath.toString images.day21.book) ] [] ]
                            , button [ type_ "button", onClick (Try Chair), class "answer-image" ] [ img [ src (ImagePath.toString images.day21.chair) ] [] ]
                            , button [ type_ "button", onClick (Try Mouse), class "answer-image" ] [ img [ src (ImagePath.toString images.day21.mouse) ] [] ]
                            , button [ type_ "button", onClick (Try Bottle), class "answer-image" ] [ img [ src (ImagePath.toString images.day21.bottle) ] [] ]
                            ]
                        ]

                InProgress (StepResult result model) ->
                    let
                        enigma =
                            Nonempty.head model.enigmas
                    in
                    if result then
                        div [ css [ color Colors.primary ] ]
                            [ typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Bravo, c'était la bonne réponse !"
                            , p [ css [ textAlign center, marginTop Spacing.L ] ] [ button [ type_ "button", onClick GoToNextEnigma ] [ text "Continuer" ] ]
                            ]

                    else
                        div [ css [ color Colors.secondary ] ]
                            [ typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Dommage, ce n'était pas la bonne réponse... Voici la bonne réponse :"
                            , p [ css [ textAlign center, marginTop Spacing.M ] ] [ img [ src enigma.image, alt "Image de l'énigme", class "enigma-image" ] [] ]
                            , div [ class "answer-image", css [ textAlign center ] ]
                                [ case enigma.answer of
                                    Ghost ->
                                        img [ src (ImagePath.toString images.day21.ghost) ] []

                                    Book ->
                                        img [ src (ImagePath.toString images.day21.book) ] []

                                    Chair ->
                                        img [ src (ImagePath.toString images.day21.chair) ] []

                                    Mouse ->
                                        img [ src (ImagePath.toString images.day21.mouse) ] []

                                    Bottle ->
                                        img [ src (ImagePath.toString images.day21.bottle) ] []
                                , p [ css [ textAlign center, marginTop Spacing.L ] ] [ button [ type_ "button", onClick GoToNextEnigma ] [ text "Continuer" ] ]
                                ]
                            ]

                Done _ ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] ("Défi terminé ! Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 21 ]
                        , p [] [ twitterLink 21 ]
                        ]
            , typography Paragraph p [ css [ marginTop Spacing.L, fontWeight bold, textAlign center ] ] "Un énorme merci à Laure qui a fourni les images d'aujourd'hui ! 🎉"
            , homeLink
            ]


isDone : Model -> Bool
isDone model =
    case model of
        ReadingInstructions ->
            False

        Done _ ->
            True

        InProgress _ ->
            False


saveState : Model -> Encode.Value
saveState state =
    case state of
        ReadingInstructions ->
            Encode.object [ ( "state", Encode.string "instructions" ) ]

        InProgress (Step model) ->
            Encode.object
                [ ( "state", Encode.string "in-progress" )
                , ( "score", Encode.int model.score )
                , ( "remaining", Encode.int (Nonempty.length model.enigmas) )
                ]

        InProgress (StepResult _ model) ->
            case Nonempty.tail model.enigmas of
                _ :: others ->
                    Encode.object
                        [ ( "state", Encode.string "in-progress" )
                        , ( "score", Encode.int model.score )
                        , ( "remaining", Encode.int (List.length others + 1) )
                        ]

                [] ->
                    Encode.object
                        [ ( "state", Encode.string "done" )
                        , ( "score", Encode.int model.score )
                        ]

        Done score ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "score", Encode.int score )
                ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "instructions" ->
                        Decode.succeed ReadingInstructions

                    "in-progress" ->
                        Decode.map2
                            (\score remaining ->
                                let
                                    allEnigmas =
                                        firstEnigma :: otherEnigmas

                                    remainingEnigmas =
                                        List.drop (List.length allEnigmas - remaining) (firstEnigma :: otherEnigmas)
                                in
                                case remainingEnigmas of
                                    first :: others ->
                                        Step { score = score, enigmas = Nonempty.Nonempty first others }
                                            |> InProgress
                                            |> Decode.succeed

                                    [] ->
                                        Decode.fail "Invalid state"
                            )
                            (Decode.field "score" Decode.int)
                            (Decode.field "remaining" Decode.int)
                            |> Decode.andThen identity

                    "done" ->
                        Decode.map Done (Decode.field "score" Decode.int)

                    stateValue ->
                        Decode.fail ("Unknown state value: " ++ stateValue)
            )


styles : List Snippet
styles =
    [ Css.class "items"
        [ displayFlex
        , justifyContent center
        ]
    , Css.class "answer-image"
        [ padding Spacing.NoSpace
        , backgroundColor transparent
        , borderColor transparent
        , Css.descendants
            [ Css.img
                [ width (px 120)
                , height (px 210)
                ]
            ]
        ]
    , Css.class "enigma-image"
        [ width (px 200)
        ]
    ]
