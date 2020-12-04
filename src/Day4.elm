module Day4 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (bold, center, color, cursor, displayFlex, flexWrap, fontWeight, justifyContent, listStyle, none, pointer, right, spaceAround, textAlign, wrap)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Colors as Colors
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (SpacingSize(..), marginBottom, marginTop, paddingLeft)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, form, h1, input, li, ol, p, span, text, ul)
import Html.Styled.Attributes exposing (class, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)
import Utils.Html exposing (nothing, viewMaybe)


type alias Enigma =
    { question : String
    , firstClue : String
    , secondClue : String
    , thirdClue : String
    , answers : Nonempty String
    }


type CluesNumber
    = One
    | Two
    | Three


type EnigmaState
    = CluesNumberChoice
    | FirstTry CluesNumber String
    | SecondTry CluesNumber String String
    | ThirdTry CluesNumber String String String


type Model
    = FirstQuestion { enigma : Enigma, state : EnigmaState }
    | OtherQuestions { previousResults : Nonempty ( Enigma, CluesNumber, Bool ), current : Enigma, currentState : EnigmaState, remaining : List Enigma }
    | Done (List ( Enigma, CluesNumber, Bool ))


init : Model
init =
    FirstQuestion { state = CluesNumberChoice, enigma = firstEnigma }


firstEnigma : Enigma
firstEnigma =
    { question = "Quelle pâtisserie ?"
    , firstClue = "Elle doit son existence à deux sœurs."
    , secondClue = "Elle doit son succès à une erreur de préparation."
    , thirdClue = "Elle peut être servie accompagnée de glace à la vanille ou de crème fraîche."
    , answers = Nonempty "La tarte Tatin" [ "tarte Tatin", "Tatin", "une tarte Tatin" ]
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { question = "Quel film français ?"
      , firstClue = "Il connaît un succès mitigé en salle mais les rediffusions TV l'ont consacré."
      , secondClue = "Il est réalisé par Jean-Marie Poiré."
      , thirdClue = "Il est joué par la Troupe du Splendid."
      , answers = Nonempty "Le Père Noël est une ordure" [ "Le père-noël est une ordure", "père noël est une ordure", "père-noël est une ordure" ]
      }
    , { question = "Quel célèbre personnage de fiction ?"
      , firstClue = "Il est l'un des plus célèbres méchants de la littérature jeunesse contemporaine."
      , secondClue = "Les gens ont peur de prononcer son nom."
      , thirdClue = "Il est défait par Harry Potter."
      , answers = Nonempty "Voldemort" [ "Voldemor", "Voldemord", "Lord Voldemort", "Lord Voldemor", "Lord Voldemord", "Tom Jedusor", "Tom Elvis Jedusor", "Jedusor" ]
      }
    , { question = "Quel mammifère ?"
      , firstClue = "Il fait partie du genre des vulpes."
      , secondClue = "Il donne son nom à un célèbre recueil de récits médiévaux."
      , thirdClue = "Il demande au Petit Prince de l'apprivoiser."
      , answers = Nonempty "Le renard" [ "renard", "un renard" ]
      }
    , { question = "Quel célèbre acteur américain ?"
      , firstClue = "Il est également musicien et son premier album s'appelle \"The Return of Bruno\"."
      , secondClue = "Il est l'un des chauves les plus célèbres du cinéma."
      , thirdClue = "John McClane est son rôle le plus connu."
      , answers = Nonempty "Bruce Willis" [ "Willis", "Bruce Wilis", "Wilis" ]
      }
    ]


type Msg
    = Try String
    | FieldChanged String
    | CluesNumberChosen CluesNumber


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
                        , currentState = CluesNumberChoice
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
                        , currentState = CluesNumberChoice
                        , remaining = others
                        }

                LastEnigmaDone result ->
                    Done (result :: Nonempty.toList otherQuestionsState.previousResults)

        Done doneState ->
            Done doneState


type UpdateResult
    = SameEnigma EnigmaState
    | NextEnigma Enigma (List Enigma) ( Enigma, CluesNumber, Bool )
    | LastEnigmaDone ( Enigma, CluesNumber, Bool )


updateState : Enigma -> EnigmaState -> List Enigma -> Msg -> UpdateResult
updateState enigma state nextEnigmas msg =
    case ( state, msg ) of
        ( CluesNumberChoice, CluesNumberChosen cluesNumber ) ->
            SameEnigma (FirstTry cluesNumber "")

        ( _, CluesNumberChosen _ ) ->
            SameEnigma state

        ( FirstTry cluesNumber _, Try answer ) ->
            if List.member (normalize answer) (Nonempty.map normalize enigma.answers |> Nonempty.toList) then
                case nextEnigmas of
                    first :: remaining ->
                        NextEnigma first remaining ( enigma, cluesNumber, True )

                    [] ->
                        LastEnigmaDone ( enigma, cluesNumber, True )

            else
                SameEnigma (SecondTry cluesNumber "" answer)

        ( SecondTry cluesNumber _ firstWrongAnswer, Try answer ) ->
            if List.member (normalize answer) (Nonempty.map normalize enigma.answers |> Nonempty.toList) then
                case nextEnigmas of
                    first :: remaining ->
                        NextEnigma first remaining ( enigma, cluesNumber, True )

                    [] ->
                        LastEnigmaDone ( enigma, cluesNumber, True )

            else
                SameEnigma (ThirdTry cluesNumber "" firstWrongAnswer answer)

        ( ThirdTry cluesNumber _ _ _, Try answer ) ->
            let
                result =
                    List.member (normalize answer) (Nonempty.map normalize enigma.answers |> Nonempty.toList)
            in
            case nextEnigmas of
                first :: remaining ->
                    NextEnigma first remaining ( enigma, cluesNumber, result )

                [] ->
                    LastEnigmaDone ( enigma, cluesNumber, result )

        ( CluesNumberChoice, Try _ ) ->
            SameEnigma state

        ( FirstTry cluesNumberChoice _, FieldChanged newValue ) ->
            SameEnigma (FirstTry cluesNumberChoice newValue)

        ( SecondTry cluesNumberChoice _ firstWrongAnswer, FieldChanged newValue ) ->
            SameEnigma (SecondTry cluesNumberChoice newValue firstWrongAnswer)

        ( ThirdTry cluesNumberChoice _ firstWrongAnswer secondWrongAnswer, FieldChanged newValue ) ->
            SameEnigma (ThirdTry cluesNumberChoice newValue firstWrongAnswer secondWrongAnswer)

        ( CluesNumberChoice, FieldChanged _ ) ->
            SameEnigma state


getScore : Model -> Int
getScore state =
    case state of
        Done results ->
            List.map getQuestionScore results |> List.sum

        FirstQuestion _ ->
            0

        OtherQuestions { previousResults } ->
            Nonempty.map getQuestionScore previousResults
                |> Nonempty.toList
                |> List.sum


getQuestionScore : ( Enigma, CluesNumber, Bool ) -> Int
getQuestionScore ( _, cluesNumber, result ) =
    case ( cluesNumber, result ) of
        ( _, False ) ->
            0

        ( One, True ) ->
            3

        ( Two, True ) ->
            2

        ( Three, True ) ->
            1


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
                Time.toDay zone currentDate
    in
    if maxDay < 4 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day4" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 4 | iKnow"
            , case state of
                Done previousResults ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ viewMaybe viewQuestionResult (List.head previousResults)
                        , typography HeroText p [] ("Défi terminé ! Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 4 ]
                        , p [] [ twitterLink 4 ]
                        ]

                FirstQuestion model ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "A partir d'une question très vague, choisissez le nombre d'indices que vous souhaitez avoir pour trouver la bonne réponse. Les derniers indices sont généralement les plus faciles mais attention ! Plus vous demandez d'indices et moins vous gagnerez de points !"
                        , typography Instructions p [ css [ textAlign center ] ] "Vous aurez ensuite trois essais pour tenter de trouver la bonne réponse..."
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.S ] ] ("Question : " ++ model.enigma.question)
                        , case model.state of
                            CluesNumberChoice ->
                                viewClueNumberChoice

                            FirstTry cluesNumber fieldValue ->
                                div []
                                    [ viewClues cluesNumber firstEnigma
                                    , viewForm ThreeTries fieldValue
                                    ]

                            SecondTry cluesNumber fieldValue _ ->
                                div []
                                    [ viewClues cluesNumber firstEnigma
                                    , viewForm TwoTries fieldValue
                                    ]

                            ThirdTry cluesNumber fieldValue _ _ ->
                                div []
                                    [ viewClues cluesNumber firstEnigma
                                    , viewForm OneTry fieldValue
                                    ]
                        ]

                OtherQuestions model ->
                    div []
                        [ case model.currentState of
                            CluesNumberChoice ->
                                viewQuestionResult (Nonempty.head model.previousResults)

                            _ ->
                                nothing
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.S ] ] ("Question : " ++ model.current.question)
                        , case model.currentState of
                            CluesNumberChoice ->
                                viewClueNumberChoice

                            FirstTry cluesNumber fieldValue ->
                                div []
                                    [ viewClues cluesNumber model.current
                                    , viewForm ThreeTries fieldValue
                                    ]

                            SecondTry cluesNumber fieldValue _ ->
                                div []
                                    [ viewClues cluesNumber model.current
                                    , viewForm TwoTries fieldValue
                                    ]

                            ThirdTry cluesNumber fieldValue _ _ ->
                                div []
                                    [ viewClues cluesNumber model.current
                                    , viewForm OneTry fieldValue
                                    ]
                        ]
            , homeLink
            ]


viewQuestionResult : ( Enigma, CluesNumber, Bool ) -> Html msg
viewQuestionResult ( enigma, cluesNumber, isCorrect ) =
    if isCorrect then
        let
            points =
                case cluesNumber of
                    One ->
                        "3 points"

                    Two ->
                        "2 points"

                    Three ->
                        "1 point"
        in
        p [ css [ marginTop Spacing.M, marginBottom Spacing.M, textAlign center ] ]
            [ typography Paragraph span [ css [ color Colors.primary ] ] ("Bravo ! La réponse était bien : " ++ Nonempty.head enigma.answers ++ ".")
            , typography Paragraph span [ css [ color Colors.primary ] ] (" Vous avez gagné " ++ points ++ " !")
            ]

    else
        p [ css [ marginTop Spacing.M, marginBottom Spacing.M, textAlign center ] ]
            [ typography Paragraph span [ css [ color Colors.secondary ] ] ("Dommage... La bonne réponse était : " ++ Nonempty.head enigma.answers ++ ".")
            ]


viewClueNumberChoice : Html Msg
viewClueNumberChoice =
    p [ css [ textAlign center ] ]
        [ typography Paragraph span [] "Combien voulez-vous d'indices ?"
        , ul [ class "clues-number-choice" ]
            [ li [] [ typography Paragraph button [ type_ "button", onClick (CluesNumberChosen One) ] "Un indice (3 points)" ]
            , li [] [ typography Paragraph button [ type_ "button", onClick (CluesNumberChosen Two) ] "Deux indices (2 points)" ]
            , li [] [ typography Paragraph button [ type_ "button", onClick (CluesNumberChosen Three) ] "Trois indices (1 point)" ]
            ]
        ]


viewClues : CluesNumber -> Enigma -> Html msg
viewClues cluesNumber enigma =
    ol []
        [ typography Paragraph li [] enigma.firstClue
        , if cluesNumber == Two || cluesNumber == Three then
            typography Paragraph li [] enigma.secondClue

          else
            nothing
        , if cluesNumber == Three then
            typography Paragraph li [] enigma.thirdClue

          else
            nothing
        ]


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
        CluesNumberChoice ->
            Encode.object [ ( "step", Encode.string "clues-number-choice" ) ]

        FirstTry cluesNumber _ ->
            Encode.object [ ( "step", Encode.string "first-try" ), ( "clues-number", encodeCluesNumber cluesNumber ) ]

        SecondTry cluesNumber _ firstWrongAnswer ->
            Encode.object
                [ ( "step", Encode.string "second-try" )
                , ( "clues-number", encodeCluesNumber cluesNumber )
                , ( "first-wrong-answer", Encode.string firstWrongAnswer )
                ]

        ThirdTry cluesNumber _ firstWrongAnswer secondWrongAnswer ->
            Encode.object
                [ ( "step", Encode.string "third-try" )
                , ( "clues-number", encodeCluesNumber cluesNumber )
                , ( "first-wrong-answer", Encode.string firstWrongAnswer )
                , ( "second-wrong-answer", Encode.string secondWrongAnswer )
                ]


encodeCluesNumber : CluesNumber -> Encode.Value
encodeCluesNumber cluesNumber =
    case cluesNumber of
        One ->
            Encode.int 1

        Two ->
            Encode.int 2

        Three ->
            Encode.int 3


encodeResults : List ( Enigma, CluesNumber, Bool ) -> Encode.Value
encodeResults previousResults =
    Encode.list
        (\( _, cluesNumber, result ) ->
            Encode.object
                [ ( "clues-number", encodeCluesNumber cluesNumber )
                , ( "result", Encode.bool result )
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
                    "clues-number-choice" ->
                        Decode.succeed CluesNumberChoice

                    "first-try" ->
                        Decode.field "clues-number" cluesNumberDecoder
                            |> Decode.map (\cluesNumber -> FirstTry cluesNumber "")

                    "second-try" ->
                        Decode.map2 (\cluesNumber firstWrongAnswer -> SecondTry cluesNumber "" firstWrongAnswer)
                            (Decode.field "clues-number" cluesNumberDecoder)
                            (Decode.field "first-wrong-answer" Decode.string)

                    "third-try" ->
                        Decode.map3 (\cluesNumber firstWrongAnswer secondWrongAnswer -> ThirdTry cluesNumber "" firstWrongAnswer secondWrongAnswer)
                            (Decode.field "clues-number" cluesNumberDecoder)
                            (Decode.field "first-wrong-answer" Decode.string)
                            (Decode.field "second-wrong-answer" Decode.string)

                    _ ->
                        Decode.fail ("Invalid step: " ++ step)
            )


otherEnigmasDecoder : Decoder ( Nonempty ( Enigma, CluesNumber, Bool ), Enigma, List Enigma )
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


resultsDecoder : Decoder (Nonempty ( Enigma, CluesNumber, Bool ))
resultsDecoder =
    Decode.list
        (Decode.map2 Tuple.pair
            (Decode.field "clues-number" cluesNumberDecoder)
            (Decode.field "result" Decode.bool)
        )
        |> Decode.andThen
            (\previousResults ->
                List.zip (List.reverse previousResults) (firstEnigma :: otherEnigmas)
                    |> List.map (\( ( cluesNumber, result ), enigma ) -> ( enigma, cluesNumber, result ))
                    |> List.reverse
                    |> Nonempty.fromList
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "Invalid previous results")
            )


cluesNumberDecoder : Decoder CluesNumber
cluesNumberDecoder =
    Decode.int
        |> Decode.andThen
            (\number ->
                case number of
                    1 ->
                        Decode.succeed One

                    2 ->
                        Decode.succeed Two

                    3 ->
                        Decode.succeed Three

                    _ ->
                        Decode.fail ("Invalid value for clues number: " ++ String.fromInt number)
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
    , Css.ol
        [ Css.property "counter-reset" "list-counter"
        , listStyle none
        , paddingLeft Spacing.M
        , Css.children
            [ Css.li
                [ Css.property "counter-increment" "list-counter"
                , Css.before
                    [ Css.property "content" "\"[Indice \" counter(list-counter) \"]\\a0\""
                    , color Colors.secondary
                    , fontWeight bold
                    ]
                , Css.adjacentSiblings
                    [ Css.li [ marginTop Spacing.M ] ]
                ]
            ]
        ]
    ]
