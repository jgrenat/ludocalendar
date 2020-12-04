module Day6 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (auto, block, bold, center, color, cursor, display, displayFlex, flexWrap, fontWeight, justifyContent, listStyle, margin, maxHeight, maxWidth, none, pct, pointer, right, spaceAround, textAlign, vh, width, wrap)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Colors as Colors
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (SpacingSize(..), marginBottom, marginTop, paddingLeft)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, form, h1, img, input, li, ol, p, span, text, ul)
import Html.Styled.Attributes exposing (alt, class, css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)
import Utils.Html exposing (nothing, viewMaybe)


type alias Enigma =
    { image : String
    , answerImage : String
    , answers : Nonempty String
    }


type EnigmaState
    = FirstTry String
    | SecondTry String String
    | ThirdTry String String String
    | Answer Bool


type Model
    = FirstQuestion { enigma : Enigma, state : EnigmaState }
    | OtherQuestions { previousResults : Nonempty ( Enigma, Bool ), current : Enigma, currentState : EnigmaState, remaining : List Enigma }
    | Done (List ( Enigma, Bool ))


init : Model
init =
    FirstQuestion { state = FirstTry "", enigma = firstEnigma }


firstEnigma : Enigma
firstEnigma =
    { image = ImagePath.toString images.day6.day61Image
    , answerImage = ImagePath.toString images.day6.day61Answer
    , answers = Nonempty "Une étoile" [ "étoile", "l'étoile", "étoile de Noël", "une étoile de Noël" ]
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { image = ImagePath.toString images.day6.day61Image
      , answerImage = ImagePath.toString images.day6.day61Answer
      , answers = Nonempty "Une étoile" [ "étoile", "l'étoile", "étoile de Noël", "une étoile de Noël" ]
      }
    , { image = ImagePath.toString images.day6.day61Image
      , answerImage = ImagePath.toString images.day6.day61Answer
      , answers = Nonempty "La tarte Tatin" [ "tarte Tatin", "Tatin", "une tarte Tatin" ]
      }
    ]


type Msg
    = Try String
    | FieldChanged String
    | NextQuestion


normalize : String -> String
normalize string =
    String.replace " " "" string
        |> removeDiacritics
        |> String.toLower


update : Model -> Msg -> Model
update modelState msg =
    case modelState of
        FirstQuestion { enigma, state } ->
            let
                updatedResults =
                    updateState enigma state otherEnigmas msg
            in
            case updatedResults of
                SameEnigma enigmaState ->
                    FirstQuestion { enigma = enigma, state = enigmaState }

                NextEnigma nextEnigma others result ->
                    OtherQuestions
                        { previousResults = Nonempty.fromElement result
                        , current = nextEnigma
                        , currentState = FirstTry ""
                        , remaining = others
                        }

                LastEnigmaDone result ->
                    Done [ result ]

        OtherQuestions otherQuestionsState ->
            let
                updatedResults =
                    updateState otherQuestionsState.current otherQuestionsState.currentState otherQuestionsState.remaining msg
            in
            case updatedResults of
                SameEnigma enigmaState ->
                    OtherQuestions { otherQuestionsState | currentState = enigmaState }

                NextEnigma nextEnigma others result ->
                    OtherQuestions
                        { previousResults = Nonempty.cons result otherQuestionsState.previousResults
                        , current = nextEnigma
                        , currentState = FirstTry ""
                        , remaining = others
                        }

                LastEnigmaDone result ->
                    Done (result :: Nonempty.toList otherQuestionsState.previousResults)

        Done doneState ->
            Done doneState


type UpdateResult
    = SameEnigma EnigmaState
    | NextEnigma Enigma (List Enigma) ( Enigma, Bool )
    | LastEnigmaDone ( Enigma, Bool )


updateState : Enigma -> EnigmaState -> List Enigma -> Msg -> UpdateResult
updateState enigma state nextEnigmas msg =
    case ( state, msg ) of
        ( FirstTry _, Try answer ) ->
            if List.member (normalize answer) (Nonempty.map normalize enigma.answers |> Nonempty.toList) then
                SameEnigma (Answer True)

            else
                SameEnigma (SecondTry "" answer)

        ( SecondTry _ firstWrongAnswer, Try answer ) ->
            if List.member (normalize answer) (Nonempty.map normalize enigma.answers |> Nonempty.toList) then
                SameEnigma (Answer True)

            else
                SameEnigma (ThirdTry "" firstWrongAnswer answer)

        ( ThirdTry _ _ _, Try answer ) ->
            let
                result =
                    List.member (normalize answer) (Nonempty.map normalize enigma.answers |> Nonempty.toList)
            in
            SameEnigma (Answer result)

        ( Answer _, Try _ ) ->
            SameEnigma state

        ( FirstTry _, FieldChanged newValue ) ->
            SameEnigma (FirstTry newValue)

        ( SecondTry _ firstWrongAnswer, FieldChanged newValue ) ->
            SameEnigma (SecondTry newValue firstWrongAnswer)

        ( ThirdTry _ firstWrongAnswer secondWrongAnswer, FieldChanged newValue ) ->
            SameEnigma (ThirdTry newValue firstWrongAnswer secondWrongAnswer)

        ( Answer _, FieldChanged _ ) ->
            SameEnigma state

        ( Answer result, NextQuestion ) ->
            case nextEnigmas of
                first :: remaining ->
                    NextEnigma first remaining ( enigma, result )

                [] ->
                    LastEnigmaDone ( enigma, result )

        ( _, NextQuestion ) ->
            SameEnigma state


getScore : Model -> Int
getScore modelState =
    case modelState of
        Done results ->
            List.map getQuestionScore results |> List.sum

        FirstQuestion { state } ->
            case state of
                Answer True ->
                    5

                _ ->
                    0

        OtherQuestions { currentState, previousResults } ->
            let
                previousResultsScore =
                    Nonempty.map getQuestionScore previousResults
                        |> Nonempty.toList
                        |> List.sum
            in
            case currentState of
                Answer True ->
                    previousResultsScore + 5

                _ ->
                    previousResultsScore


getQuestionScore : ( Enigma, Bool ) -> Int
getQuestionScore ( _, result ) =
    if result then
        5

    else
        0


type RemainingTries
    = ThreeTries
    | TwoTries
    | OneTry


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate + 3
    in
    if maxDay < 6 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day4" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 4 | iKnow"
            , case state of
                Done previousResults ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ viewMaybe (\( enigma, result ) -> viewQuestionResult enigma result) (List.head previousResults)
                        , typography HeroText p [] ("Défi terminé ! Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 6 ]
                        , p [] [ twitterLink 6 ]
                        ]

                FirstQuestion model ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Sur chacun des dessins suivants, il manque un élément. Saurez-vous l'identifier ?"
                        , typography Instructions p [ css [ textAlign center ] ] "Vous aurez trois essais pour tenter de trouver la bonne réponse..."
                        , typography Paragraph p [ css [ textAlign center, marginTop Spacing.XS ] ] "Un peu d'indulgence, je suis loin d'être un artiste ! 😅"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.S ] ] "Qu'est-ce qu'il manque à l'image suivante ?"
                        , case model.state of
                            FirstTry fieldValue ->
                                div []
                                    [ viewImage firstEnigma.image
                                    , viewForm ThreeTries fieldValue
                                    ]

                            SecondTry fieldValue _ ->
                                div []
                                    [ viewImage firstEnigma.image
                                    , viewForm TwoTries fieldValue
                                    ]

                            ThirdTry fieldValue _ _ ->
                                div []
                                    [ viewImage firstEnigma.image
                                    , viewForm OneTry fieldValue
                                    ]

                            Answer result ->
                                div []
                                    [ viewImage firstEnigma.answerImage
                                    , viewQuestionResult firstEnigma result
                                    , div [ css [ textAlign center, marginTop Spacing.M ] ]
                                        [ button [ type_ "button", onClick NextQuestion ] [ text "Question suivante" ]
                                        ]
                                    ]
                        ]

                OtherQuestions model ->
                    div []
                        [ typography HeroText p [ css [ textAlign center, marginBottom Spacing.S ] ] "Qu'est-ce qu'il manque à l'image suivante ?"
                        , case model.currentState of
                            FirstTry fieldValue ->
                                div []
                                    [ viewImage model.current.image
                                    , viewForm ThreeTries fieldValue
                                    ]

                            SecondTry fieldValue _ ->
                                div []
                                    [ viewImage model.current.image
                                    , viewForm TwoTries fieldValue
                                    ]

                            ThirdTry fieldValue _ _ ->
                                div []
                                    [ viewImage model.current.image
                                    , viewForm OneTry fieldValue
                                    ]

                            Answer result ->
                                let
                                    buttonLabel =
                                        case model.remaining of
                                            [] ->
                                                "Résultat"

                                            _ ->
                                                "Question suivante"
                                in
                                div []
                                    [ viewImage model.current.answerImage
                                    , viewQuestionResult model.current result
                                    , div [ css [ textAlign center, marginTop Spacing.M ] ]
                                        [ button [ onClick NextQuestion ] [ text buttonLabel ]
                                        ]
                                    ]
                        ]
            , homeLink
            ]


viewQuestionResult : Enigma -> Bool -> Html msg
viewQuestionResult enigma isCorrect =
    if isCorrect then
        p [ css [ marginTop Spacing.M, marginBottom Spacing.M, textAlign center ] ]
            [ typography Paragraph span [ css [ color Colors.primary ] ] ("Bravo ! La réponse était bien : " ++ Nonempty.head enigma.answers ++ ".")
            , typography Paragraph span [ css [ color Colors.primary ] ] " Vous avez gagné 5 points !"
            ]

    else
        p [ css [ marginTop Spacing.M, marginBottom Spacing.M, textAlign center ] ]
            [ typography Paragraph span [ css [ color Colors.secondary ] ] ("Dommage... La bonne réponse était : " ++ Nonempty.head enigma.answers ++ ".")
            ]


viewImage : String -> Html msg
viewImage image =
    img [ class "image", src image, alt "image de l'énigme" ] []


viewForm : RemainingTries -> String -> Html Msg
viewForm remainingTries fieldValue =
    form
        [ onSubmit (Try fieldValue)
        , css
            [ textAlign center
            , marginTop Spacing.M
            ]
        ]
        [ case remainingTries of
            ThreeTries ->
                typography Paragraph p [] "3 essais restants"

            TwoTries ->
                typography Paragraph p [] "2 essais restants"

            OneTry ->
                typography Paragraph p [] "Dernier essai"
        , input [ type_ "text", value fieldValue, onInput FieldChanged ] []
        , button [ type_ "submit" ] [ text "Valider" ]
        , typography Instructions p [ css [ textAlign center, marginTop Spacing.S ] ] "(Les accents et majuscules ne sont pas importants.)"
        ]


isDone : Model -> Bool
isDone model =
    case model of
        Done _ ->
            True

        _ ->
            False


saveState : Model -> Encode.Value
saveState modelState =
    case modelState of
        FirstQuestion { state } ->
            Encode.object [ ( "state", Encode.string "first-question" ), ( "enigma-state", encodeEnigmaState state ) ]

        OtherQuestions { currentState, previousResults } ->
            Encode.object
                [ ( "state", Encode.string "other-questions" )
                , ( "enigma-state", encodeEnigmaState currentState )
                , ( "previous-results", encodeResults (Nonempty.toList previousResults) )
                ]

        Done results ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "results", encodeResults results )
                ]


encodeEnigmaState : EnigmaState -> Encode.Value
encodeEnigmaState enigmaState =
    case enigmaState of
        FirstTry _ ->
            Encode.object [ ( "step", Encode.string "first-try" ) ]

        SecondTry _ firstWrongAnswer ->
            Encode.object
                [ ( "step", Encode.string "second-try" )
                , ( "first-wrong-answer", Encode.string firstWrongAnswer )
                ]

        ThirdTry _ firstWrongAnswer secondWrongAnswer ->
            Encode.object
                [ ( "step", Encode.string "third-try" )
                , ( "first-wrong-answer", Encode.string firstWrongAnswer )
                , ( "second-wrong-answer", Encode.string secondWrongAnswer )
                ]

        Answer result ->
            Encode.object
                [ ( "step", Encode.string "answer" )
                , ( "result", Encode.bool result )
                ]


encodeResults : List ( Enigma, Bool ) -> Encode.Value
encodeResults previousResults =
    Encode.list
        (\( _, result ) ->
            Encode.object
                [ ( "result", Encode.bool result )
                ]
        )
        previousResults


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "first-question" ->
                        Decode.map (\enigmaState -> FirstQuestion { enigma = firstEnigma, state = enigmaState }) (Decode.field "enigma-state" enigmaStateDecoder)

                    "other-questions" ->
                        Decode.map2 (\enigmaState ( previousResults, current, remaining ) -> OtherQuestions { current = current, currentState = enigmaState, previousResults = previousResults, remaining = remaining })
                            (Decode.field "enigma-state" enigmaStateDecoder)
                            otherEnigmasDecoder

                    "done" ->
                        Decode.map Done (Decode.field "results" resultsDecoder |> Decode.map Nonempty.toList)

                    _ ->
                        Decode.fail "Invalid state"
            )


enigmaStateDecoder : Decoder EnigmaState
enigmaStateDecoder =
    Decode.field "step" Decode.string
        |> Decode.andThen
            (\step ->
                case step of
                    "first-try" ->
                        FirstTry "" |> Decode.succeed

                    "second-try" ->
                        Decode.map (SecondTry "")
                            (Decode.field "first-wrong-answer" Decode.string)

                    "third-try" ->
                        Decode.map2 (ThirdTry "")
                            (Decode.field "first-wrong-answer" Decode.string)
                            (Decode.field "second-wrong-answer" Decode.string)

                    "answer" ->
                        Decode.map Answer (Decode.field "result" Decode.bool)

                    _ ->
                        Decode.fail ("Invalid step: " ++ step)
            )


otherEnigmasDecoder : Decoder ( Nonempty ( Enigma, Bool ), Enigma, List Enigma )
otherEnigmasDecoder =
    Decode.andThen
        (\previousResults ->
            case List.drop (Nonempty.length previousResults) (firstEnigma :: otherEnigmas) of
                first :: others ->
                    Decode.succeed ( previousResults, first, others )

                [] ->
                    Decode.fail "Too many results in the previous results list"
        )
        (Decode.field "previous-results" resultsDecoder)


resultsDecoder : Decoder (Nonempty ( Enigma, Bool ))
resultsDecoder =
    Decode.list (Decode.field "result" Decode.bool)
        |> Decode.andThen
            (\previousResults ->
                List.zip (List.reverse previousResults) (firstEnigma :: otherEnigmas)
                    |> List.map (\( result, enigma ) -> ( enigma, result ))
                    |> List.reverse
                    |> Nonempty.fromList
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "Invalid previous results")
            )


styles : List Snippet
styles =
    [ Css.class "clues-number-choice"
        [ displayFlex
        , flexWrap wrap
        , justifyContent spaceAround
        , marginBottom Spacing.M
        , marginTop Spacing.S
        , Css.descendants
            [ Css.button
                [ cursor pointer
                , marginBottom Spacing.XS
                ]
            ]
        ]
    , Css.class "image"
        [ maxWidth (pct 100)
        , maxHeight (vh 70)
        , display block
        , margin auto
        ]
    ]
