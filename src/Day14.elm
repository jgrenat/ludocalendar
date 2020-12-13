module Day14 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (absolute, alignItems, backgroundColor, borderColor, borderRadius, borderStyle, center, dashed, displayFlex, em, fontSize, height, int, justifyContent, left, pct, position, px, relative, rgb, rgba, textAlign, top, width, zIndex, zero)
import Css.Global as Css exposing (Snippet)
import Css.Media as Media
import DesignSystem.Colors as Colors
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop, paddingBottom, paddingLeft, paddingRight, paddingTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Dict exposing (Dict)
import Html.Styled exposing (Html, button, div, form, h1, input, p, text)
import Html.Styled.Attributes exposing (class, css, disabled, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import List.Nonempty as Nonempty exposing (Nonempty(..))
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)


type alias Enigma =
    { grid : List (List String)
    , firstQueue : Nonempty String
    , secondQueue : Nonempty String
    , firstWord : String
    , secondWord : String
    , answers : Nonempty String
    , question : String
    }


type Model
    = ReadingInstructions
    | FirstEnigma EnigmaState
    | SecondEnigma Int EnigmaState
    | DayDone Int


type Queue
    = NotAllFound (Nonempty String) (Nonempty String)
    | AllFound (List String)


type EnigmaState
    = Ricochet { firstQueue : Queue, secondQueue : Queue, failedWords : List String, failedAttempts : Int, selectedWord : Maybe String }
    | FindingWord { failedAttempts : Int, fieldValue : String, lastAttempt : Maybe String }
    | Done { failedAttempts : Int, result : Bool }


init : Model
init =
    ReadingInstructions


firstEnigma : Enigma
firstEnigma =
    { grid =
        [ [ "canard", "dentiste", "sandale", "Noël", "couteau" ]
        , [ "coquillage", "ancien", "couronne", "seize", "neuf" ]
        , [ "yeux", "4x4", "nombre", "tropézienne", "oiseau" ]
        , [ "chevalier", "chaussure", "nourriture", "roi", "gâteau" ]
        , [ "voiture", "moule", "joie", "kiwi", "combattant" ]
        ]
    , firstWord = "laqué"
    , secondWord = "volant"
    , firstQueue = Nonempty "canard" [ "oiseau", "kiwi", "nourriture", "couteau", "coquillage", "moule", "gâteau", "tropézienne", "sandale", "chaussure" ]
    , secondQueue = Nonempty "voiture" [ "4x4", "seize", "nombre", "neuf", "ancien", "combattant", "chevalier", "roi", "couronne", "dentiste" ]
    , answers = Nonempty "Joyeux Noël !" [ "joyeux noël", "joie yeux noël" ]
    , question = "Qu'est-ce qu'on va bientôt se souhaiter ?"
    }


secondEnigma : Enigma
secondEnigma =
    { grid =
        [ [ "hase", "mie", "souris", "piano", "ver" ]
        , [ "pâtisserie", "zut", "solitaire", "ouvrir", "zumba" ]
        , [ "dé", "laque", "pain", "métier", "opéra" ]
        , [ "coudre", "cuisinière", "flûte", "danse", "clavier" ]
        , [ "rat", "tisser", "jeu", "chenille", "boulangerie" ]
        ]
    , firstWord = "électroménager"
    , secondWord = "épouse"
    , firstQueue = Nonempty "mie" [ "pain", "boulangerie", "pâtisserie", "opéra", "rat", "souris", "clavier", "piano", "flûte", "zut" ]
    , secondQueue = Nonempty "cuisinière" [ "métier", "tisser", "coudre", "dé", "jeu", "solitaire", "ver", "chenille", "danse", "zumba" ]
    , answers = Nonempty "Ouvrir la case !" [ "ouvrir la case", "ouvrir une case", "ouvrir laque hase" ]
    , question = "Que faut-il faire chaque jour sur son calendrier de l'Avent ?"
    }


type Msg
    = Start
    | TryWord String
    | TryAnswer String
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
            Ricochet
                { firstQueue = NotAllFound (Nonempty.fromElement firstEnigma.firstWord) firstEnigma.firstQueue
                , secondQueue = NotAllFound (Nonempty.fromElement firstEnigma.secondWord) firstEnigma.secondQueue
                , failedWords = []
                , failedAttempts = 0
                , selectedWord = Nothing
                }
                |> FirstEnigma

        ( ReadingInstructions, _ ) ->
            ReadingInstructions

        ( FirstEnigma enigmaState, _ ) ->
            case updateState firstEnigma enigmaState msg of
                SameEnigma newState ->
                    FirstEnigma newState

                NextEnigma score ->
                    Ricochet
                        { firstQueue = NotAllFound (Nonempty.fromElement secondEnigma.firstWord) secondEnigma.firstQueue
                        , secondQueue = NotAllFound (Nonempty.fromElement secondEnigma.secondWord) secondEnigma.secondQueue
                        , failedWords = []
                        , failedAttempts = 0
                        , selectedWord = Nothing
                        }
                        |> SecondEnigma score

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
        ( Ricochet model, TryWord word ) ->
            if isWordNextInQueues word model.firstQueue model.secondQueue then
                case model.selectedWord of
                    Nothing ->
                        Ricochet { model | selectedWord = Just word }
                            |> SameEnigma

                    Just _ ->
                        case ( nextWord model.firstQueue, nextWord model.secondQueue ) of
                            ( AllFound _, AllFound _ ) ->
                                FindingWord
                                    { failedAttempts = model.failedAttempts
                                    , fieldValue = ""
                                    , lastAttempt = Nothing
                                    }
                                    |> SameEnigma

                            ( newFirstQueue, newSecondQueue ) ->
                                Ricochet
                                    { model
                                        | firstQueue = newFirstQueue
                                        , secondQueue = newSecondQueue
                                        , failedWords = []
                                        , selectedWord = Nothing
                                    }
                                    |> SameEnigma

            else
                Ricochet { model | failedWords = word :: model.failedWords, failedAttempts = model.failedAttempts + 1 }
                    |> SameEnigma

        ( Ricochet _, _ ) ->
            SameEnigma state

        ( FindingWord model, FieldChanged newValue ) ->
            FindingWord { model | fieldValue = newValue }
                |> SameEnigma

        ( FindingWord model, TryAnswer answer ) ->
            if Nonempty.member (normalize answer) (Nonempty.map normalize enigma.answers) then
                Done { failedAttempts = model.failedAttempts, result = True }
                    |> SameEnigma

            else
                FindingWord { model | failedAttempts = model.failedAttempts + 1, fieldValue = "", lastAttempt = Just answer }
                    |> SameEnigma

        ( FindingWord _, _ ) ->
            SameEnigma state

        ( Done _, StartNextEnigma ) ->
            NextEnigma (getStateScore state)

        ( Done _, _ ) ->
            SameEnigma state


nextWord : Queue -> Queue
nextWord queue =
    case queue of
        NotAllFound foundWords (Nonempty first []) ->
            AllFound (first :: Nonempty.toList foundWords)

        NotAllFound foundWords (Nonempty first (firstRemaining :: remaining)) ->
            NotAllFound (Nonempty.cons first foundWords) (Nonempty firstRemaining remaining)

        AllFound _ ->
            queue


isWordNextInQueues : String -> Queue -> Queue -> Bool
isWordNextInQueues word firstQueue secondQueue =
    isWordNextInQueue word firstQueue || isWordNextInQueue word secondQueue


isWordNextInQueue : String -> Queue -> Bool
isWordNextInQueue word queue =
    case queue of
        NotAllFound _ remainingWords ->
            Nonempty.head remainingWords == word

        AllFound _ ->
            False


getStateScore : EnigmaState -> Int
getStateScore enigmaState =
    case enigmaState of
        Done { failedAttempts, result } ->
            if result then
                if failedAttempts < 3 then
                    10

                else if failedAttempts < 6 then
                    7

                else if failedAttempts < 9 then
                    5

                else
                    3

            else
                0

        Ricochet _ ->
            0

        FindingWord _ ->
            0


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


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay < 14 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 14 | Ricochet"
            , case state of
                ReadingInstructions ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "A partir des deux mots de départ, vous devez ricocher de mot en mot jusqu'à n'avoir plus que trois mots sur la grille ! Pour cela, associez chacun des deux mots de départ avec l'un des mots de la grille. Pour vous aider, les deux mots correspondants sont soit sur la même ligne, soit sur la même colonne."
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.XS ] ] "Les associations peuvent être des synonymes (nuit -> soir), des contraires (nuit -> jour), des expressions (nuit -> blanche) ou des mots du même champs lexical (nuit -> ombre)."
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Assemblez les trois derniers mots dans le bon ordre pour former phonétiquement une expression ou une phrase répondant à la question posée."
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.XS ] ] "Par exemple, s'il vous reste les mots \"lard\", \"lire\" et \"aigle\" pour répondre à la question \"Que fait-on avant de jouer ?\", vous pourrez répondre \"Lire la règle !\" (lire, lard, aigle)."
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
                        [ typography HeroText p [] "L'épreuve du jour est terminée, félicitations !"
                        , typography HeroText p [ css [ marginTop Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 14 ]
                        , p [] [ twitterLink 14 ]
                        ]
            , homeLink
            ]


viewEnigma : Enigma -> EnigmaState -> Html Msg
viewEnigma enigma enigmaState =
    case enigmaState of
        Ricochet model ->
            div []
                [ typography HeroText p [ css [ textAlign center, marginBottom Spacing.XS ] ] ("Quels sont les ricochets des mots \"" ++ getLastWord model.firstQueue ++ "\" et \"" ++ getLastWord model.secondQueue ++ "\" ?")
                , typography Instructions p [ css [ textAlign center, marginBottom Spacing.M ] ] "Souvenez-vous : ils sont soit tous les deux sur la même ligne, soit sur la même colonne !"
                , viewGrid enigma.grid model.firstQueue model.secondQueue model.failedWords model.selectedWord
                ]

        FindingWord model ->
            div [ css [ textAlign center ] ]
                [ viewGrid enigma.grid (AllFound (Nonempty.toList enigma.firstQueue)) (AllFound (Nonempty.toList enigma.secondQueue)) [] Nothing
                , typography Instructions p [ css [ marginTop Spacing.L ] ] "En mettant les trois mots restant dans le bon ordre, vous retrouverez phonétiquement une phrase pour répondre à la question suivante :"
                , typography HeroText p [ css [ marginTop Spacing.S ] ] enigma.question
                , form [ onSubmit (TryAnswer model.fieldValue) ]
                    [ input [ type_ "text", value model.fieldValue, onInput FieldChanged, css [ marginTop Spacing.M ] ] []
                    , button [ type_ "submit" ] [ text "Répondre" ]
                    ]
                ]

        Done _ ->
            div []
                [ typography HeroText p [ css [ textAlign center ] ] ("Bravo, l'expression à trouver était bien : " ++ Nonempty.head enigma.answers)
                , typography HeroText p [ css [ textAlign center, marginTop Spacing.M ] ] ("Points gagnés : " ++ String.fromInt (getStateScore enigmaState))
                , p [ css [ textAlign center, marginTop Spacing.L ] ] [ button [ type_ "button", onClick StartNextEnigma ] [ text "Continuer" ] ]
                ]


getLastWord : Queue -> String
getLastWord queue =
    case queue of
        NotAllFound foundWords _ ->
            Nonempty.head foundWords

        AllFound strings ->
            ""


viewGrid : List (List String) -> Queue -> Queue -> List String -> Maybe String -> Html Msg
viewGrid grid firstQueue secondQueue failedWords selectedWord =
    let
        foundWords =
            Dict.union (getFoundWordsWithNumber firstQueue) (getFoundWordsWithNumber secondQueue)
    in
    List.concat grid
        |> List.map (viewWord foundWords failedWords selectedWord)
        |> div [ class "grid" ]


getFoundWordsWithNumber : Queue -> Dict String Int
getFoundWordsWithNumber queue =
    case queue of
        NotAllFound foundWords _ ->
            Nonempty.toList foundWords
                |> List.reverse
                |> List.indexedMap (\index word -> ( word, index ))
                |> Dict.fromList

        AllFound words ->
            List.indexedMap (\index word -> ( word, index )) words
                |> Dict.fromList


viewWord : Dict String Int -> List String -> Maybe String -> String -> Html Msg
viewWord foundWords failedAttempts selectedWord word =
    case Dict.get word foundWords of
        Nothing ->
            if selectedWord == Just word then
                button [ class "word selectedWord", disabled True ] [ text word ]

            else if List.member word failedAttempts then
                button [ class "word failedWord", disabled True ] [ text word ]

            else
                button [ class "word", onClick (TryWord word) ] [ text word ]

        Just order ->
            button [ class ("word foundWord-" ++ String.fromInt order), disabled True ] [ text word ]


isDone : Model -> Bool
isDone model =
    case model of
        DayDone _ ->
            True

        _ ->
            False


saveState : Model -> Encode.Value
saveState modelState =
    case modelState of
        ReadingInstructions ->
            Encode.object [ ( "state", Encode.string "reading-instructions" ) ]

        FirstEnigma enigmaState ->
            Encode.object
                [ ( "state", Encode.string "first-enigma" )
                , ( "enigma-state", encodeEnigmaState enigmaState )
                ]

        SecondEnigma firstEnigmaScore enigmaState ->
            Encode.object
                [ ( "state", Encode.string "second-enigma" )
                , ( "first-enigma-score", Encode.int firstEnigmaScore )
                , ( "enigma-state", encodeEnigmaState enigmaState )
                ]

        DayDone score ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "score", Encode.int score )
                ]


encodeEnigmaState : EnigmaState -> Encode.Value
encodeEnigmaState enigmaState =
    case enigmaState of
        Ricochet model ->
            Encode.object
                [ ( "step", Encode.string "ricochet" )
                , ( "first-queue", encodeQueue model.firstQueue )
                , ( "second-queue", encodeQueue model.secondQueue )
                , ( "failed-words", Encode.list Encode.string model.failedWords )
                , ( "failed-attempts", Encode.int model.failedAttempts )
                , ( "selected-word", Encode.maybe Encode.string model.selectedWord )
                ]

        FindingWord model ->
            Encode.object
                [ ( "step", Encode.string "finding-word" )
                , ( "failed-attempts", Encode.int model.failedAttempts )
                ]

        Done model ->
            Encode.object
                [ ( "step", Encode.string "done" )
                , ( "failed-attempts", Encode.int model.failedAttempts )
                , ( "result", Encode.bool model.result )
                ]


encodeQueue : Queue -> Encode.Value
encodeQueue queue =
    case queue of
        NotAllFound foundWords remaining ->
            Encode.object
                [ ( "state", Encode.string "not-all-found" )
                , ( "found-words", Nonempty.toList foundWords |> Encode.list Encode.string )
                , ( "remaining-words", Nonempty.toList remaining |> Encode.list Encode.string )
                ]

        AllFound words ->
            Encode.object
                [ ( "state", Encode.string "all-found" )
                , ( "words", Encode.list Encode.string words )
                ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "reading-instructions" ->
                        Decode.succeed ReadingInstructions

                    "first-enigma" ->
                        Decode.map FirstEnigma (Decode.field "enigma-state" enigmaStateDecoder)

                    "second-enigma" ->
                        Decode.map2 SecondEnigma
                            (Decode.field "first-enigma-score" Decode.int)
                            (Decode.field "enigma-state" enigmaStateDecoder)

                    "done" ->
                        Decode.map DayDone (Decode.field "score" Decode.int)

                    _ ->
                        Decode.fail "Invalid state"
            )


enigmaStateDecoder : Decoder EnigmaState
enigmaStateDecoder =
    Decode.field "step" Decode.string
        |> Decode.andThen
            (\step ->
                case step of
                    "ricochet" ->
                        Decode.map5
                            (\firstQueue secondQueue failedWords failedAttempts selectedWord ->
                                Ricochet
                                    { firstQueue = firstQueue
                                    , secondQueue = secondQueue
                                    , failedWords = failedWords
                                    , failedAttempts = failedAttempts
                                    , selectedWord = selectedWord
                                    }
                            )
                            (Decode.field "first-queue" queueDecoder)
                            (Decode.field "second-queue" queueDecoder)
                            (Decode.field "failed-words" (Decode.list Decode.string))
                            (Decode.field "failed-attempts" Decode.int)
                            (Decode.field "selected-word" (Decode.maybe Decode.string))

                    "finding-word" ->
                        Decode.map (\failedAttempts -> FindingWord { failedAttempts = failedAttempts, fieldValue = "", lastAttempt = Nothing })
                            (Decode.field "failed-attempts" Decode.int)

                    "done" ->
                        Decode.map2 (\failedAttempts result -> Done { failedAttempts = failedAttempts, result = result })
                            (Decode.field "failed-attempts" Decode.int)
                            (Decode.field "result" Decode.bool)

                    _ ->
                        Decode.fail ("Invalid step: " ++ step)
            )


queueDecoder : Decoder Queue
queueDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "not-all-found" ->
                        Decode.map2 NotAllFound
                            (Decode.field "found-words" (nonEmptyListDecoder Decode.string))
                            (Decode.field "remaining-words" (nonEmptyListDecoder Decode.string))

                    "all-found" ->
                        Decode.map AllFound (Decode.list Decode.string)

                    invalidState ->
                        Decode.fail ("Invalid queue state: " ++ invalidState)
            )


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
    [ Css.class "grid"
        [ Css.property "display" "grid"
        , Css.property "grid-template-columns" "repeat(5, calc(20% - 0.8vw) [col-start]);"
        , Css.property "row-gap" "1vw"
        , Css.property "column-gap" "1vw"
        ]
    , Css.class "word"
        [ paddingTop Spacing.S
        , paddingBottom Spacing.S
        , paddingLeft Spacing.NoSpace
        , paddingRight Spacing.NoSpace
        , borderRadius (pct 50)
        , backgroundColor (rgb 155 87 252)
        , borderColor (rgb 103 19 220)
        , Css.focus
            [ backgroundColor (rgb 188 151 241)
            , borderColor (rgb 103 19 220)
            , borderStyle dashed
            ]
        , Media.withMedia [ Media.all [ Media.maxWidth (px 360) ] ]
            [ fontSize (em 0.7) ]
        , Css.withClass "selectedWord"
            [ backgroundColor Colors.primary ]
        , Css.withClass "failedWord"
            [ backgroundColor Colors.secondary ]
        , Css.withAttribute "class*=foundWord"
            [ position relative
            , Css.before
                [ Css.property "content" "''"
                , position absolute
                , width (pct 100)
                , height (pct 100)
                , top zero
                , left zero
                , displayFlex
                , justifyContent center
                , alignItems center
                , backgroundColor (rgba 0 0 0 0.8)
                , fontSize (em 1.5)
                , zIndex (int 1)
                , borderRadius (pct 50)
                ]
            , Css.withClass "foundWord-1"
                [ Css.before [ Css.property "content" "'1'" ]
                ]
            , Css.withClass "foundWord-2"
                [ Css.before [ Css.property "content" "'2'" ]
                ]
            , Css.withClass "foundWord-3"
                [ Css.before [ Css.property "content" "'3'" ]
                ]
            , Css.withClass "foundWord-4"
                [ Css.before [ Css.property "content" "'4'" ]
                ]
            , Css.withClass "foundWord-5"
                [ Css.before [ Css.property "content" "'5'" ]
                ]
            , Css.withClass "foundWord-6"
                [ Css.before [ Css.property "content" "'6'" ]
                ]
            , Css.withClass "foundWord-7"
                [ Css.before [ Css.property "content" "'7'" ]
                ]
            , Css.withClass "foundWord-8"
                [ Css.before [ Css.property "content" "'8'" ]
                ]
            , Css.withClass "foundWord-9"
                [ Css.before [ Css.property "content" "'9'" ]
                ]
            , Css.withClass "foundWord-10"
                [ Css.before [ Css.property "content" "'10'" ]
                ]
            , Css.withClass "foundWord-11"
                [ Css.before [ Css.property "content" "'11'" ]
                ]
            ]
        ]
    ]
