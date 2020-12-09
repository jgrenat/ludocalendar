module Day10 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (Pct, Px, absolute, alignItems, backgroundColor, backgroundImage, backgroundPosition2, backgroundRepeat, backgroundSize, border3, borderColor, borderRadius, borderRightColor, center, color, contain, cursor, display, displayFlex, flexWrap, height, inlineBlock, int, justifyContent, left, noRepeat, notAllowed, outline, outline3, pct, pointer, position, px, relative, right, solid, spaceAround, textAlign, top, transform, translateY, transparent, url, vh, wrap, zIndex, zero)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Colors as Colors exposing (white)
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (SpacingSize(..), marginBottom, marginLeft, marginRight, marginTop, padding2)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, h1, li, p, text)
import Html.Styled.Attributes exposing (class, css, type_)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import Time exposing (Posix, Zone)


type alias Event =
    { label : String
    , date : String
    }


type alias Clue =
    { label : String, value : Int }


type alias Enigma =
    { clue1 : Clue
    , clue2 : Clue
    , possibleAnswers : List String
    , answer : String
    }


firstEnigma : Enigma
firstEnigma =
    { clue1 = Clue "Je l'inviterais volontiers au restaurant" 3 -- 3
    , clue2 = Clue "Fréquente les salons de beauté" 5 -- 0
    , possibleAnswers = [ "Le Père-Noël", "Un pirate", "Un étudiant en informatique", "Dracula", "Dark Vador" ]
    , answer = "Un étudiant en informatique"
    }


secondEnigma : Enigma
secondEnigma =
    { clue1 = Clue "A beaucoup de choses à dire chez le psy" 5
    , clue2 = Clue "Laisse sa place aux mamies dans le bus" -5
    , possibleAnswers = [ "Bob l'éponge", "Le Capitaine Crochet", "Un supporter de foot", "Georges Clooney", "James Bond" ]
    , answer = "Le Capitaine Crochet"
    }


thirdEnigma : Enigma
thirdEnigma =
    { clue1 = Clue "Fait tapisserie en boîte de nuit" -5
    , clue2 = Clue "J'aimerais me retrouver sur une île déserte avec lui/elle" 3
    , possibleAnswers = [ "Le Diable", "Une rockstar", "Tony Stark, Iron Man", "Une princesse", "Le Dalaï Lama" ]
    , answer = "Une rockstar"
    }


type EnigmaState
    = InProgress { eliminatedCharacters : List String, remainingCharacters : List String }
    | Lost Int
    | Won Int


type Model
    = FirstEnigma EnigmaState
    | SecondEnigma Int EnigmaState
    | ThirdEnigma Int Int EnigmaState
    | DayDone Int


init : Model
init =
    FirstEnigma (InProgress { eliminatedCharacters = [], remainingCharacters = firstEnigma.possibleAnswers })


type Msg
    = Try String
    | Continue


update : Model -> Msg -> Model
update modelState msg =
    case modelState of
        FirstEnigma state ->
            case updateState firstEnigma state msg of
                SameEnigma newState ->
                    FirstEnigma newState

                NextEnigma score ->
                    SecondEnigma score (InProgress { eliminatedCharacters = [], remainingCharacters = secondEnigma.possibleAnswers })

        SecondEnigma firstEnigmaScore state ->
            case updateState secondEnigma state msg of
                SameEnigma newState ->
                    SecondEnigma firstEnigmaScore newState

                NextEnigma score ->
                    ThirdEnigma firstEnigmaScore score (InProgress { eliminatedCharacters = [], remainingCharacters = thirdEnigma.possibleAnswers })

        ThirdEnigma firstEnigmaScore secondEnigmaScore state ->
            case updateState thirdEnigma state msg of
                SameEnigma newState ->
                    ThirdEnigma firstEnigmaScore secondEnigmaScore newState

                NextEnigma score ->
                    DayDone (firstEnigmaScore + secondEnigmaScore + score)

        DayDone score ->
            DayDone score


type UpdateResult
    = SameEnigma EnigmaState
    | NextEnigma Int


updateState : Enigma -> EnigmaState -> Msg -> UpdateResult
updateState enigma state msg =
    case ( state, msg ) of
        ( InProgress { eliminatedCharacters, remainingCharacters }, Try character ) ->
            if character /= enigma.answer then
                let
                    newRemainingCharacters =
                        List.filter ((/=) character) remainingCharacters
                in
                case newRemainingCharacters of
                    _ :: [] ->
                        SameEnigma (Won 8)

                    _ ->
                        SameEnigma
                            (InProgress
                                { remainingCharacters = newRemainingCharacters
                                , eliminatedCharacters = character :: eliminatedCharacters
                                }
                            )

            else
                SameEnigma (Lost (List.length eliminatedCharacters * 2))

        ( _, Try _ ) ->
            SameEnigma state

        ( Won score, Continue ) ->
            NextEnigma score

        ( Lost score, Continue ) ->
            NextEnigma score

        ( InProgress _, Continue ) ->
            SameEnigma state


getScore : Model -> Int
getScore state =
    case state of
        DayDone score ->
            score

        FirstEnigma enigmaState ->
            getEnigmaStateScore enigmaState

        SecondEnigma firstEnigmaScore enigmaState ->
            firstEnigmaScore + getEnigmaStateScore enigmaState

        ThirdEnigma firstEnigmaScore secondEnigmaScore enigmaState ->
            firstEnigmaScore + secondEnigmaScore + getEnigmaStateScore enigmaState


getEnigmaStateScore : EnigmaState -> Int
getEnigmaStateScore enigmaState =
    case enigmaState of
        InProgress { eliminatedCharacters } ->
            List.length eliminatedCharacters * 2

        Lost score ->
            score

        Won score ->
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
    if maxDay < 10 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day4" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 10 | Profiler"
            , case state of
                FirstEnigma enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Trouvez le bon personnage sur les cinq proposés. Pour cela, deux indices sont posés sur une règle graduée, 5 signifiant que l'on est d'accord avec l'indice et -5 signifiant que nous ne somme pas du tout d'accord avec l'indice."
                        , typography Instructions p [ css [ textAlign center ] ] "Pour trouver le bon personnage, vous allez devoir éliminer un à un les personnages incorrects, commencez par les plus évidents !"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , viewClues firstEnigma.clue1 firstEnigma.clue2
                        , viewEnigma firstEnigma enigmaState
                        ]

                SecondEnigma _ enigmaState ->
                    div []
                        [ typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , viewClues secondEnigma.clue1 secondEnigma.clue2
                        , viewEnigma secondEnigma enigmaState
                        ]

                ThirdEnigma _ _ enigmaState ->
                    div []
                        [ typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , viewClues thirdEnigma.clue1 thirdEnigma.clue2
                        , viewEnigma thirdEnigma enigmaState
                        ]

                DayDone _ ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] "Le défi du jour est terminé !"
                        , typography HeroText p [ css [ marginTop Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 10 ]
                        , p [] [ twitterLink 10 ]
                        ]
            , homeLink
            ]


viewClues : Clue -> Clue -> Html msg
viewClues clue1 clue2 =
    div [ class "clues-ruler" ]
        [ div [ class "clue", css [ top (getClueOffset clue1) ] ] [ text clue1.label ]
        , div [ class "clue", css [ top (getClueOffset clue2) ] ] [ text clue2.label ]
        ]


viewEnigma : Enigma -> EnigmaState -> Html Msg
viewEnigma enigma enigmaState =
    case enigmaState of
        InProgress { remainingCharacters, eliminatedCharacters } ->
            div []
                [ typography Instructions p [ css [ textAlign center, marginBottom Spacing.S ] ] "Éliminez les personnages qui ne correspondent PAS aux indices ci-dessus :"
                , Keyed.ul [ class "characters" ]
                    (List.map viewCharacter remainingCharacters)
                ]

        Won _ ->
            div []
                [ typography HeroText p [ css [ textAlign center ] ] ("Bravo ! Le bon personnage était bien : " ++ enigma.answer)
                , p [ css [ textAlign center, marginTop Spacing.L ] ] [ button [ type_ "button", onClick Continue ] [ text "Continuer" ] ]
                ]

        Lost _ ->
            div []
                [ typography HeroText p [ css [ textAlign center ] ] ("Dommage ! Vous avez éliminé le bon personnage : " ++ enigma.answer)
                , p [ css [ textAlign center, marginTop Spacing.L ] ] [ button [ type_ "button", onClick Continue ] [ text "Continuer" ] ]
                ]


getClueOffset : Clue -> Pct
getClueOffset clue =
    if clue.value == -5 then
        pct 95

    else if clue.value == -4 then
        pct 84.5

    else if clue.value == -3 then
        pct 74

    else if clue.value == -2 then
        pct 65

    else if clue.value == -1 then
        pct 56

    else if clue.value == 1 then
        pct 37

    else if clue.value == 2 then
        pct 28

    else if clue.value == 3 then
        pct 21

    else if clue.value == 4 then
        pct 12

    else if clue.value == 5 then
        pct 3

    else
        pct 47


viewCharacter : String -> ( String, Html Msg )
viewCharacter character =
    ( character, li [] [ button [ type_ "button", class "character", onClick (Try character) ] [ text character ] ] )


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

        ThirdEnigma firstEnigmaScore secondEnigmaScore enigmaState ->
            Encode.object
                [ ( "state", Encode.string "third-enigma" )
                , ( "first-enigma-score", Encode.int firstEnigmaScore )
                , ( "second-enigma-score", Encode.int secondEnigmaScore )
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
        InProgress { remainingCharacters, eliminatedCharacters } ->
            Encode.object
                [ ( "step", Encode.string "in-progress" )
                , ( "remaining-characters", Encode.list Encode.string remainingCharacters )
                , ( "eliminated-characters", Encode.list Encode.string eliminatedCharacters )
                ]

        Won score ->
            Encode.object
                [ ( "step", Encode.string "won" )
                , ( "score", Encode.int score )
                ]

        Lost score ->
            Encode.object
                [ ( "step", Encode.string "lost" )
                , ( "score", Encode.int score )
                ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "first-enigma" ->
                        Decode.map FirstEnigma (Decode.field "enigma-state" (enigmaStateDecoder firstEnigma))

                    "second-enigma" ->
                        Decode.map2 SecondEnigma
                            (Decode.field "first-enigma-score" Decode.int)
                            (Decode.field "enigma-state" (enigmaStateDecoder secondEnigma))

                    "third-enigma" ->
                        Decode.map3 ThirdEnigma
                            (Decode.field "first-enigma-score" Decode.int)
                            (Decode.field "second-enigma-score" Decode.int)
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
                    "in-progress" ->
                        Decode.map2 (\remainingCharacters eliminatedCharacters -> InProgress { remainingCharacters = remainingCharacters, eliminatedCharacters = eliminatedCharacters })
                            (Decode.field "remaining-characters" (Decode.list Decode.string))
                            (Decode.field "eliminated-characters" (Decode.list Decode.string))

                    "won" ->
                        Decode.map Won (Decode.field "score" Decode.int)

                    "lost" ->
                        Decode.map Lost (Decode.field "score" Decode.int)

                    _ ->
                        Decode.fail ("Invalid step: " ++ step)
            )


styles : List Snippet
styles =
    [ Css.class "characters"
        [ displayFlex
        , alignItems center
        , justifyContent spaceAround
        , flexWrap wrap
        , Css.descendants
            [ Css.class "character"
                [ marginTop Spacing.M
                , marginLeft Spacing.S
                , marginRight Spacing.S
                , cursor pointer
                ]
            ]
        ]
    , Css.class "clues-ruler"
        [ height (vh 50)
        , backgroundImage (ImagePath.toString images.day10.profilerRuler |> url)
        , backgroundRepeat noRepeat
        , backgroundSize contain
        , backgroundPosition2 zero zero
        , position relative
        , marginBottom Spacing.L
        , Css.children
            [ Css.class "clue"
                [ Css.padding2 (vh 2) (vh 3)
                , backgroundColor Colors.secondary
                , color Colors.white
                , display inlineBlock
                , position absolute
                , left (vh 10)
                , transform (translateY (pct -50))
                , borderRadius (px 5)
                , Css.before
                    [ Css.property "content" "''"
                    , position absolute
                    , top (pct 50)
                    , Css.marginTop (px -5)
                    , left (px -10)
                    , border3 (px 5) solid transparent
                    , borderRightColor Colors.secondary
                    , zIndex (int 1)
                    ]
                ]
            ]
        ]
    ]
