module Day11 exposing (Model, Msg, init, isDone, saveState, stateDecoder, subscriptions, update, view)

import Css exposing (center, color, textAlign)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Colors as Colors
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, br, button, div, form, h1, input, li, p, span, text, ul)
import Html.Styled.Attributes exposing (class, classList, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import String.Extra as String
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)


type alias Enigma =
    { questions : Nonempty Question }


type alias Question =
    { label : String, answers : Nonempty String }


type Model
    = ReadingInstructions
    | FirstEnigma EnigmaState
    | SecondEnigma Int EnigmaState
    | DayDone Int


type EnigmaState
    = QuestionsBeingAsked { done : List Question, current : Question, remaining : List Question }
    | Answers { done : List ( Question, Maybe String, Bool ), current : Question, remaining : List Question, fieldValue : String }
    | Done (List ( Question, Maybe String, Bool ))


init : Model
init =
    ReadingInstructions


firstEnigma : Enigma
firstEnigma =
    { questions =
        Nonempty { label = "Vrai ou faux : LudoCalendar est le meilleur des calendriers de l'Avent !", answers = Nonempty "Vrai (mais \"faux\" était accepté 😢)" [ "vrai", "faux", "oui", "non" ] }
            [ { label = "Quel est le jour de Noël ? (indiquez juste le numéro du jour)", answers = Nonempty "25" [ "25 décembre", "le 25", "le 25 décembre" ] }
            , { label = "Qu'est-ce qui est le plus lourd ? Un kilo de plumes ou un kilo de plomb ? S'ils ont le même poids, répondez \"ananas\".", answers = Nonempty "ananas" [ "annanas", "anannas", "un ananas" ] }
            , { label = "De quelle couleur est le cheval blanc d'Henri IV ?", answers = Nonempty "blanc" [ "cheval blanc", "il est blanc", "blanche" ] }
            , { label = "Vrai ou faux : Un pain perdu est un pain qui ne sait pas quoi faire de sa vie.", answers = Nonempty "Faux" [ "vrai", "oui", "non" ] }
            , { label = "C'est l'histoire d'un tétard qui croyait qu'il était tôt alors qu'en fait il était tard. Est-ce que cette blague vous a fait rire ? Répondez \"oui\" ou \"non\"", answers = Nonempty "Oui (\"non\" accepté 😢)" [ "oui", "non" ] }
            , { label = "Combien font 7 + 3 ?", answers = Nonempty "10" [ "dix" ] }
            , { label = "Quel est le prénom de Chabat, l'animateur vedette de Burger Quiz ?", answers = Nonempty "Alain" [ "Alain Chabat" ] }
            , { label = "Quel est le nom de famille d'Alain, l'animateur vedette de Burger Quiz ?", answers = Nonempty "Chabat" [ "Alain Chabat", "Chabbat", "Chaba", "Chabba" ] }
            , { label = "Vrai ou faux : cette question est la dixième question.", answers = Nonempty "Vrai" [] }
            ]
    }


secondEnigma : Enigma
secondEnigma =
    { questions =
        Nonempty { label = "Est-ce que le raton laveur est un animal maniaque ? Répondez par \"oui\" ou \"non\"", answers = Nonempty "Non" [ "oui" ] }
            [ { label = "Combien seraient les sept nains si on en enlevait deux ?", answers = Nonempty "5" [ "cinq" ] }
            , { label = "Et si on en enlevait encore un ?", answers = Nonempty "4" [ "quatre" ] }
            , { label = "Quelle est la cinquième lettre de l'alphabet ? Si vous ne savez pas, dîtes \"heu\".", answers = Nonempty "E" [ "La lettre E", "Heu", "Heuu", "Heuuu" ] }
            , { label = "Vous préférez le thé ou le café ?", answers = Nonempty "Le thé ou le café" [ "le thé", "le café", "thé", "café" ] }
            , { label = "Sur une échelle de 1 à 10, à quel point êtes-vous confiant sur votre performance à ce quiz ?", answers = Nonempty "N'importe quel nombre" [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit", "neuf", "dix" ] }
            , { label = "Quel animal est Mickey ?", answers = Nonempty "Une souris" [ "souris", "une sourie", "sourie", "une souri", "souri" ] }
            , { label = "Qui est plus rapide, le lièvre ou la tortue ?", answers = Nonempty "Le lièvre ou la tortue" [ "Le lièvre", "La tortue", "lièvre", "tortue" ] }
            , { label = "Quelle est la capitale de la France ?", answers = Nonempty "Paris" [] }
            , { label = "Vrai ou faux : il y a King Kong derrière vous.", answers = Nonempty "Faux (enfin, j'espère !)" [ "Faux", "Vrai" ] }
            ]
    }


type Msg
    = Tick
    | Start
    | Answer String
    | StartNextEnigma
    | FieldChanged String


normalize : String -> String
normalize string =
    String.replace " " "" string
        |> removeDiacritics
        |> String.toLower


update : Model -> Msg -> Model
update state msg =
    case ( state, msg ) of
        ( ReadingInstructions, Start ) ->
            QuestionsBeingAsked
                { done = []
                , current = Nonempty.head firstEnigma.questions
                , remaining = Nonempty.tail firstEnigma.questions
                }
                |> FirstEnigma

        ( ReadingInstructions, _ ) ->
            ReadingInstructions

        ( FirstEnigma enigmaState, _ ) ->
            case updateState firstEnigma enigmaState msg of
                SameEnigma newState ->
                    FirstEnigma newState

                NextEnigma score ->
                    SecondEnigma score
                        (QuestionsBeingAsked
                            { done = []
                            , current = Nonempty.head secondEnigma.questions
                            , remaining = Nonempty.tail secondEnigma.questions
                            }
                        )

        ( SecondEnigma firstEnigmaScore enigmaState, _ ) ->
            case updateState secondEnigma enigmaState msg of
                SameEnigma newState ->
                    SecondEnigma firstEnigmaScore newState

                NextEnigma score ->
                    DayDone (firstEnigmaScore + score)

        ( DayDone _, _ ) ->
            state


type UpdateResult
    = SameEnigma EnigmaState
    | NextEnigma Int


updateState : Enigma -> EnigmaState -> Msg -> UpdateResult
updateState enigma state msg =
    case ( state, msg ) of
        ( Answers model, FieldChanged newValue ) ->
            Answers { model | fieldValue = newValue }
                |> SameEnigma

        ( _, FieldChanged _ ) ->
            SameEnigma state

        ( QuestionsBeingAsked model, Tick ) ->
            case model.remaining of
                first :: remaining ->
                    QuestionsBeingAsked { model | done = model.current :: model.done, current = first, remaining = remaining }
                        |> SameEnigma

                [] ->
                    let
                        questions =
                            Nonempty model.current model.done
                                |> Nonempty.reverse
                    in
                    Answers { done = [], current = Nonempty.head questions, remaining = Nonempty.tail questions, fieldValue = "" }
                        |> SameEnigma

        ( QuestionsBeingAsked _, _ ) ->
            SameEnigma state

        ( Answers model, Answer answer ) ->
            if String.isBlank answer then
                SameEnigma state

            else
                let
                    isCorrect =
                        Nonempty.member (normalize answer) (Nonempty.map normalize model.current.answers)
                in
                case model.remaining of
                    first :: remaining ->
                        Answers
                            { model
                                | remaining = remaining
                                , done = ( model.current, Just answer, isCorrect ) :: model.done
                                , fieldValue = ""
                                , current = first
                            }
                            |> SameEnigma

                    [] ->
                        Done (( model.current, Just answer, isCorrect ) :: model.done |> List.reverse)
                            |> SameEnigma

        ( Answers _, _ ) ->
            SameEnigma state

        ( Done _, StartNextEnigma ) ->
            NextEnigma (getStateScore state)

        ( Done _, _ ) ->
            SameEnigma state


getStateScore : EnigmaState -> Int
getStateScore enigmaState =
    case enigmaState of
        QuestionsBeingAsked _ ->
            0

        Answers { done } ->
            List.count (\( _, _, isCorrect ) -> isCorrect) done

        Done answers ->
            List.count (\( _, _, isCorrect ) -> isCorrect) answers


getScore : Model -> Int
getScore state =
    case state of
        ReadingInstructions ->
            0

        FirstEnigma enigmaState ->
            getStateScore enigmaState

        SecondEnigma firstEnigmaScore enigmaState ->
            firstEnigmaScore + getStateScore enigmaState

        DayDone score ->
            score


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        FirstEnigma (QuestionsBeingAsked _) ->
            Time.every 10000 (always Tick)

        SecondEnigma _ (QuestionsBeingAsked _) ->
            Time.every 10000 (always Tick)

        _ ->
            Sub.none


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay < 10 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 11 | Burger Quiz"
            , case state of
                ReadingInstructions ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Comme dans le célèbre jeu télévisé, vous voilà confronté à l'épreuve du Burger de la Mort !"
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Dix questions vont vous être posées une à une sans que vous puissiez y répondre. Puis, à la fin des questions, vous devrez donner les bonnes réponses de mémoire dans l'ordre."
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Chaque question apparaitra 10 secondes avant que la question suivante n'apparaisse. A la fin des dix questions, vous pourrez répondre !"
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Êtes-vous prêt ?"
                        , p [ css [ textAlign center, marginTop Spacing.L ] ]
                            [ button [ type_ "button", onClick Start ] [ text "Commencer !" ]
                            ]
                        ]

                FirstEnigma enigmaState ->
                    viewEnigma firstEnigma enigmaState

                SecondEnigma _ enigmaState ->
                    viewEnigma secondEnigma enigmaState

                DayDone _ ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] "L'effrayante épreuve du Burger de la Mort est terminée !"
                        , typography HeroText p [ css [ marginTop Spacing.M ] ] ("Vous n'avez pas gagné de Jeep Reuuunaigaide, mais voici votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 11 ]
                        , p [] [ twitterLink 11 ]
                        ]
            , homeLink
            ]


viewEnigma : Enigma -> EnigmaState -> Html Msg
viewEnigma enigma enigmaState =
    case enigmaState of
        QuestionsBeingAsked model ->
            let
                questionCount =
                    List.length model.done + 1
            in
            div [ css [ textAlign center ] ]
                [ typography Instructions p [ css [ marginBottom Spacing.M ] ] "(Les questions vont défiler automatiquement toutes les 10 secondes...)"
                , typography HeroText p [] (String.fromInt questionCount ++ ") " ++ model.current.label)
                ]

        Answers model ->
            let
                questionCount =
                    List.length model.done + 1
            in
            div [ css [ textAlign center ] ]
                [ typography HeroText p [] ("Répondez à la question " ++ String.fromInt questionCount ++ " :")
                , form [ onSubmit (Answer model.fieldValue) ]
                    [ input [ type_ "text", value model.fieldValue, onInput FieldChanged, css [ marginTop Spacing.M ] ] []
                    , button [ type_ "submit" ] [ text "Répondre" ]
                    ]
                ]

        Done answers ->
            div []
                [ typography HeroText p [ css [ textAlign center ] ] "Vos réponses"
                , ul [] (List.indexedMap viewAnswer answers)
                , typography HeroText p [ css [ textAlign center, marginTop Spacing.M ] ] ("Points gagnés : " ++ String.fromInt (getStateScore enigmaState))
                , p [ css [ textAlign center, marginTop Spacing.L ] ] [ button [ type_ "button", onClick StartNextEnigma ] [ text "Continuer" ] ]
                ]


viewAnswer : Int -> ( Question, Maybe String, Bool ) -> Html msg
viewAnswer index ( question, maybeAnswer, isCorrect ) =
    let
        answer =
            Maybe.withDefault "Pas de réponse" maybeAnswer
    in
    li [ class "answer", classList [ ( "answer--correct", isCorrect ), ( "answer--wrong", not isCorrect ) ] ]
        [ typography Paragraph span [] (String.fromInt (index + 1) ++ ") " ++ question.label)
        , br [] []
        , typography Paragraph span [ class "player-answer" ] ("Votre réponse : " ++ answer ++ " | Bonne réponse : " ++ Nonempty.head question.answers)
        ]


isDone : Model -> Bool
isDone model =
    case model of
        DayDone _ ->
            True

        _ ->
            False


type EnigmaNumber
    = First
    | Second


saveState : Model -> Encode.Value
saveState modelState =
    case modelState of
        ReadingInstructions ->
            Encode.object [ ( "state", Encode.string "reading-instructions" ) ]

        FirstEnigma enigmaState ->
            Encode.object
                [ ( "state", Encode.string "first-enigma" )
                , ( "enigma-state", encodeEnigmaState First enigmaState )
                ]

        SecondEnigma firstEnigmaScore enigmaState ->
            Encode.object
                [ ( "state", Encode.string "second-enigma" )
                , ( "first-enigma-score", Encode.int firstEnigmaScore )
                , ( "enigma-state", encodeEnigmaState Second enigmaState )
                ]

        DayDone score ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "score", Encode.int score )
                ]


encodeEnigmaState : EnigmaNumber -> EnigmaState -> Encode.Value
encodeEnigmaState enigmaNumber enigmaState =
    case enigmaState of
        QuestionsBeingAsked _ ->
            Encode.object
                [ ( "step", Encode.string "questions-being-asked" )
                , ( "enigma", encodeEnigmaNumber enigmaNumber )
                ]

        Answers model ->
            Encode.object
                [ ( "step", Encode.string "answers" )
                , ( "enigma", encodeEnigmaNumber enigmaNumber )
                , ( "answers", Encode.list encodeAnswer model.done )
                ]

        Done answers ->
            Encode.object
                [ ( "step", Encode.string "done" )
                , ( "answers", Encode.list encodeAnswer answers )
                ]


encodeAnswer : ( Question, Maybe String, Bool ) -> Encode.Value
encodeAnswer ( question, maybeAnswer, isCorrect ) =
    Encode.object
        [ ( "question", encodeQuestion question )
        , ( "answer", Encode.maybe Encode.string maybeAnswer )
        , ( "is-correct", Encode.bool isCorrect )
        ]


encodeQuestion : Question -> Encode.Value
encodeQuestion question =
    Encode.object
        [ ( "label", Encode.string question.label )
        , ( "answers", Encode.list Encode.string (Nonempty.toList question.answers) )
        ]


encodeEnigmaNumber : EnigmaNumber -> Encode.Value
encodeEnigmaNumber enigmaNumber =
    case enigmaNumber of
        First ->
            Encode.int 1

        Second ->
            Encode.int 2


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "reading-instructions" ->
                        Decode.succeed ReadingInstructions

                    "first-enigma" ->
                        Decode.map FirstEnigma (Decode.field "enigma-state" (enigmaStateDecoder firstEnigma))

                    "second-enigma" ->
                        Decode.map2 SecondEnigma
                            (Decode.field "first-enigma-score" Decode.int)
                            (Decode.field "enigma-state" (enigmaStateDecoder secondEnigma))

                    "done" ->
                        Decode.map DayDone (Decode.field "score" Decode.int)

                    _ ->
                        Decode.fail "Invalid state"
            )


enigmaStateDecoder : Enigma -> Decoder EnigmaState
enigmaStateDecoder enigma =
    Decode.field "step" Decode.string
        |> Decode.andThen
            (\step ->
                case step of
                    "questions-being-asked" ->
                        Decode.map
                            (\questions ->
                                QuestionsBeingAsked
                                    { done = []
                                    , current = Nonempty.head questions
                                    , remaining = Nonempty.tail questions
                                    }
                            )
                            (Decode.field "enigma" enigmasFromEnigmaNumberDecoder)

                    "answers" ->
                        Decode.map2
                            (\questions answers ->
                                let
                                    remainingQuestionsMaybe =
                                        Nonempty.toList questions
                                            |> List.drop (List.length answers)
                                            |> Nonempty.fromList
                                in
                                case remainingQuestionsMaybe of
                                    Just remainingQuestions ->
                                        Answers
                                            { done = answers
                                            , current = Nonempty.head remainingQuestions
                                            , remaining = Nonempty.tail remainingQuestions
                                            , fieldValue = ""
                                            }
                                            |> Decode.succeed

                                    Nothing ->
                                        Decode.fail "All questions have already been answered"
                            )
                            (Decode.field "enigma" enigmasFromEnigmaNumberDecoder)
                            (Decode.field "answers" (Decode.list answerDecoder))
                            |> Decode.andThen identity

                    "done" ->
                        Decode.map Done (Decode.field "answers" (Decode.list answerDecoder))

                    _ ->
                        Decode.fail ("Invalid step: " ++ step)
            )


enigmasFromEnigmaNumberDecoder : Decoder (Nonempty Question)
enigmasFromEnigmaNumberDecoder =
    Decode.int
        |> Decode.andThen
            (\enigmaNumber ->
                case enigmaNumber of
                    1 ->
                        Decode.succeed firstEnigma.questions

                    2 ->
                        Decode.succeed secondEnigma.questions

                    _ ->
                        Decode.fail "Invalid enigma number"
            )


answerDecoder : Decoder ( Question, Maybe String, Bool )
answerDecoder =
    Decode.map3 (\question answer isCorrect -> ( question, answer, isCorrect ))
        (Decode.field "question" questionDecoder)
        (Decode.field "answer" (Decode.maybe Decode.string))
        (Decode.field "is-correct" Decode.bool)


questionDecoder : Decoder Question
questionDecoder =
    Decode.map2 Question
        (Decode.field "label" Decode.string)
        (Decode.field "answers" (nonEmptyListDecoder Decode.string))


nonEmptyListDecoder : Decoder a -> Decoder (Nonempty a)
nonEmptyListDecoder decoder =
    Decode.list decoder
        |> Decode.andThen
            (\list ->
                case list of
                    first :: remaining ->
                        Nonempty.Nonempty first remaining
                            |> Decode.succeed

                    [] ->
                        Decode.fail "Empty list"
            )


styles : List Snippet
styles =
    [ Css.class "answer"
        [ marginTop Spacing.M
        , Css.withClass "answer--correct"
            [ Css.descendants
                [ Css.class "player-answer"
                    [ color Colors.primary
                    ]
                ]
            ]
        , Css.withClass "answer--wrong"
            [ Css.descendants
                [ Css.class "player-answer"
                    [ color Colors.secondary
                    ]
                ]
            ]
        ]
    ]
