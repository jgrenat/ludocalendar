module Day16 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (backgroundColor, bold, borderWidth, center, color, cursor, displayFlex, flexWrap, fontWeight, justifyContent, listStyle, none, pointer, rgb, right, spaceAround, textAlign, transparent, wrap, zero)
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
import Utils.Html exposing (nothing, viewIf, viewMaybe)


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
    | TooHardGiveMeAll


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
    { question = "Quel héros mythologique ?"
    , firstClue = "Il est le fils de Zeus et d'une mortelle."
    , secondClue = "Les studios Disney lui ont consacré un long-métrage et une série d'animaation."
    , thirdClue = "Il est célèbre pour ses Douze Travaux."
    , answers = Nonempty "Hercule" []
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { question = "Quel célèbre réalisateur et producteur américain ?"
      , firstClue = "Son premier film est THX 1138."
      , secondClue = "Il a créé la société ILM (Industrial Light and Magic)."
      , thirdClue = "Son univers de space opera est sans doute le plus célèbre du monde."
      , answers = Nonempty "George Lucas" [ "Georges Lucas", "Lucas", "Georges", "George" ]
      }
    , { question = "Quel fruit ?"
      , firstClue = "C'est le deuxième fruit le plus consommé en France."
      , secondClue = "Tintin en recherche des specimens bleus."
      , thirdClue = "Dans plusieurs langues, il porte le nom de sa couleur."
      , answers = Nonempty "Orange" [ "l'orange", "une orange", "des oranges", "les oranges", "oranges", "des orange", "les orange" ]
      }
    , { question = "Quelle créature mythologique ?"
      , firstClue = "Elle est présente sous diverses formes dans de nombreux récits et légendes partout dans le monde."
      , secondClue = "Elle est représentée sur une célèbre tapisserie française du Moyen Âge."
      , thirdClue = "Sa célèbre corne aurait la vertu de protéger du poison."
      , answers = Nonempty "Licorne" [ "la licorne", "une licorne", "les licornes", "licornes", "des licornes" ]
      }
    , { question = "Quel arbre ?"
      , firstClue = "Il est l'objet d'un des contes d'Andersen."
      , secondClue = "D'après une chanson populaire, il est le roi des forêts."
      , thirdClue = "Il est indissociable de Noël."
      , answers = Nonempty "Saping" [ "le sapin", "un sapin", "sapins", "des sapins", "les sapins" ]
      }
    , { question = "Quel site web ?"
      , firstClue = "Il est l'activité la plus agréable du mois de décembre."
      , secondClue = "Chaque jour, il apporte son lot de divertissement."
      , thirdClue = "Vous êtes actuellement dessus."
      , answers = Nonempty "LudoCalendar" [ "ludocalendar.com", "www.ludocalendar.com", "https://ludocalendar.com", "https://www.ludocalendar.com", "http://ludocalendar.com", "http://www.ludocalendar.com", "https://ludocalendar.com/", "https://www.ludocalendar.com/", "ludo calendar", "ludo-calendar" ]
      }
    ]


type Msg
    = Try String
    | FieldChanged String
    | CluesNumberChosen CluesNumber
    | ShowAllClues


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

        ( FirstTry _ fieldValue, ShowAllClues ) ->
            FirstTry TooHardGiveMeAll fieldValue |> SameEnigma

        ( SecondTry _ firstWrongAnswer fieldValue, ShowAllClues ) ->
            SecondTry TooHardGiveMeAll firstWrongAnswer fieldValue |> SameEnigma

        ( ThirdTry _ firstWrongAnswer secondWrongAnswer fieldValue, ShowAllClues ) ->
            ThirdTry TooHardGiveMeAll firstWrongAnswer secondWrongAnswer fieldValue |> SameEnigma

        ( CluesNumberChoice, ShowAllClues ) ->
            SameEnigma state

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
            4

        ( Two, True ) ->
            3

        ( Three, True ) ->
            2

        ( TooHardGiveMeAll, True ) ->
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
    if maxDay < 16 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day16" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 16 | iKnow"
            , case state of
                Done previousResults ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ viewMaybe viewQuestionResult (List.head previousResults)
                        , typography HeroText p [] ("Défi terminé ! Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 16 ]
                        , p [] [ twitterLink 16 ]
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
                                    , viewAllClues cluesNumber
                                    ]

                            SecondTry cluesNumber fieldValue _ ->
                                div []
                                    [ viewClues cluesNumber firstEnigma
                                    , viewForm TwoTries fieldValue
                                    , viewAllClues cluesNumber
                                    ]

                            ThirdTry cluesNumber fieldValue _ _ ->
                                div []
                                    [ viewClues cluesNumber firstEnigma
                                    , viewForm OneTry fieldValue
                                    , viewAllClues cluesNumber
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
                                    , viewAllClues cluesNumber
                                    ]

                            SecondTry cluesNumber fieldValue _ ->
                                div []
                                    [ viewClues cluesNumber model.current
                                    , viewForm TwoTries fieldValue
                                    , viewAllClues cluesNumber
                                    ]

                            ThirdTry cluesNumber fieldValue _ _ ->
                                div []
                                    [ viewClues cluesNumber model.current
                                    , viewForm OneTry fieldValue
                                    , viewAllClues cluesNumber
                                    ]
                        ]
            , homeLink
            ]


viewAllClues : CluesNumber -> Html Msg
viewAllClues cluesNumber =
    div [ css [ textAlign center, marginBottom Spacing.M, marginTop Spacing.M ] ]
        [ button [ class "clue-button", onClick ShowAllClues ] [ text "😅 C'est trop dur, montrez-moi tous les indices ! (vous ne pourrez gagner que 1 point)" ]
        ]
        |> viewIf (cluesNumber == One || cluesNumber == Two)


viewQuestionResult : ( Enigma, CluesNumber, Bool ) -> Html msg
viewQuestionResult ( enigma, cluesNumber, isCorrect ) =
    if isCorrect then
        let
            points =
                case cluesNumber of
                    One ->
                        "4 points"

                    Two ->
                        "3 points"

                    Three ->
                        "2 points"

                    TooHardGiveMeAll ->
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
        , if cluesNumber == Two || cluesNumber == Three || cluesNumber == TooHardGiveMeAll then
            typography Paragraph li [] enigma.secondClue

          else
            nothing
        , if cluesNumber == Three || cluesNumber == TooHardGiveMeAll then
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

        TooHardGiveMeAll ->
            Encode.int 4


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

                    4 ->
                        Decode.succeed TooHardGiveMeAll

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
    , Css.class "clue-button"
        [ backgroundColor transparent
        , borderWidth zero
        , color (rgb 0 0 0)
        , Css.hover [ backgroundColor (rgb 230 230 230) ]
        , Css.focus [ backgroundColor (rgb 210 210 210) ]
        ]
    ]
