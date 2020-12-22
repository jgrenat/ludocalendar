module Day23 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (alignItems, backgroundColor, bold, borderColor, center, color, column, display, displayFlex, em, flexDirection, fontSize, fontWeight, height, inlineBlock, justifyContent, left, px, rgb, textAlign, transparent, width)
import Css.Global as Css exposing (Snippet)
import Day18Dice exposing (Die(..), checkScore)
import DesignSystem.Colors as Colors
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginLeft, marginRight, marginTop, paddingBottom, paddingTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, a, button, div, h1, img, li, p, section, span, text, ul)
import Html.Styled.Attributes exposing (alt, class, css, href, src, type_)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as List
import Pages exposing (images, pages)
import Pages.ImagePath as ImagePath
import Pages.PagePath as PagePath
import Time exposing (Posix, Zone)
import Utils.Html exposing (viewIf, viewMaybe)


type alias Enigma =
    { warriors : List Die }


type Model
    = FirstEnigma EnigmaState
    | SecondEnigma Int EnigmaState
    | ThirdEnigma Int EnigmaState
    | FourthEnigma Int EnigmaState
    | FifthEnigma Int EnigmaState
    | DayDone Int


type alias EnigmaState =
    { firstGroup : List Die, secondGroup : List Die, failedAttempts : Int, lastFailedAttemptScores : Maybe ( Int, Int ) }


init : Model
init =
    FirstEnigma { firstGroup = List.take 4 firstEnigma.warriors, secondGroup = List.drop 4 firstEnigma.warriors, failedAttempts = 0, lastFailedAttemptScores = Nothing }


firstEnigma : Enigma
firstEnigma =
    { warriors = [ Hero, Captain, Soldier, Soldier, Soldier, Magus, Magus ] }


secondEnigma : Enigma
secondEnigma =
    { warriors = [ Soldier, Cursed, Cursed, Magus, Magus, Magus, Magus ] }


thirdEnigma : Enigma
thirdEnigma =
    { warriors = [ Hero, Captain, Captain, Cursed, Magus, Magus, Magus ] }


fourthEnigma : Enigma
fourthEnigma =
    { warriors = [ Hero, Soldier, Traitor, Traitor, Magus, Magus, Magus ] }


fifthEnigma : Enigma
fifthEnigma =
    { warriors = [ Hero, Hero, Soldier, Soldier, Traitor, Magus, Magus ] }


type Msg
    = ValidateAnswer
    | MoveToGroup1 Die
    | MoveToGroup2 Die


update : Model -> Msg -> Model
update state msg =
    case ( state, msg ) of
        ( FirstEnigma enigmaState, _ ) ->
            case updateState enigmaState msg of
                SameEnigma newState ->
                    FirstEnigma newState

                NextEnigma score ->
                    SecondEnigma score
                        { firstGroup = List.take 4 secondEnigma.warriors
                        , secondGroup = List.drop 4 secondEnigma.warriors
                        , failedAttempts = 0
                        , lastFailedAttemptScores = Nothing
                        }

        ( SecondEnigma firstEnigmaScore enigmaState, _ ) ->
            case updateState enigmaState msg of
                SameEnigma newState ->
                    SecondEnigma firstEnigmaScore newState

                NextEnigma score ->
                    ThirdEnigma (firstEnigmaScore + score)
                        { firstGroup = List.take 4 thirdEnigma.warriors
                        , secondGroup = List.drop 4 thirdEnigma.warriors
                        , failedAttempts = 0
                        , lastFailedAttemptScores = Nothing
                        }

        ( ThirdEnigma firstEnigmasScore enigmaState, _ ) ->
            case updateState enigmaState msg of
                SameEnigma newState ->
                    ThirdEnigma firstEnigmasScore newState

                NextEnigma score ->
                    FourthEnigma (firstEnigmasScore + score)
                        { firstGroup = List.take 4 fourthEnigma.warriors
                        , secondGroup = List.drop 4 fourthEnigma.warriors
                        , failedAttempts = 0
                        , lastFailedAttemptScores = Nothing
                        }

        ( FourthEnigma firstEnigmasScore enigmaState, _ ) ->
            case updateState enigmaState msg of
                SameEnigma newState ->
                    FourthEnigma firstEnigmasScore newState

                NextEnigma score ->
                    FifthEnigma (firstEnigmasScore + score)
                        { firstGroup = List.take 4 fifthEnigma.warriors
                        , secondGroup = List.drop 4 fifthEnigma.warriors
                        , failedAttempts = 0
                        , lastFailedAttemptScores = Nothing
                        }

        ( FifthEnigma firstEnigmasScore enigmaState, _ ) ->
            case updateState enigmaState msg of
                SameEnigma newState ->
                    FifthEnigma firstEnigmasScore newState

                NextEnigma score ->
                    DayDone (firstEnigmasScore + score)

        ( DayDone _, _ ) ->
            state


type UpdateResult
    = SameEnigma EnigmaState
    | NextEnigma Int


updateState : EnigmaState -> Msg -> UpdateResult
updateState state msg =
    case msg of
        ValidateAnswer ->
            case checkScore state.firstGroup state.secondGroup of
                Day18Dice.Success _ ->
                    NextEnigma (5 - state.failedAttempts |> max 0)

                Day18Dice.Failure scores ->
                    SameEnigma { state | failedAttempts = state.failedAttempts + 1, lastFailedAttemptScores = Just scores }

        MoveToGroup1 dice ->
            if List.member dice state.secondGroup then
                SameEnigma { state | firstGroup = dice :: state.firstGroup, secondGroup = List.remove dice state.secondGroup, lastFailedAttemptScores = Nothing }

            else
                SameEnigma state

        MoveToGroup2 dice ->
            if List.member dice state.firstGroup then
                SameEnigma { state | secondGroup = dice :: state.secondGroup, firstGroup = List.remove dice state.firstGroup, lastFailedAttemptScores = Nothing }

            else
                SameEnigma state


getScore : Model -> Int
getScore state =
    case state of
        FirstEnigma _ ->
            0

        SecondEnigma firstEnigmaScore _ ->
            firstEnigmaScore

        ThirdEnigma firstEnigmasScore _ ->
            firstEnigmasScore

        FourthEnigma firstEnigmasScore _ ->
            firstEnigmasScore

        FifthEnigma firstEnigmasScore _ ->
            firstEnigmasScore

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
    if maxDay < 23 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 23 | Par Odin !"
            , case state of
                FirstEnigma enigmaState ->
                    div []
                        [ p [ css [ textAlign center ] ]
                            [ typography Instructions span [] "En tant que Dieu nordique, vous continuez à vous distraire en équilibrant les armées... Si vous n'avez pas encore fait le jour 18, "
                            , typography Instructions a [ href (PagePath.toString pages.day18) ] " je vous invite à vous y rendre pour comprendre les règles."
                            ]
                        , viewDiceRecap
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Utilisez les flèches pour répartir les combattants en deux armées de même valeur."
                        , viewEnigma enigmaState
                        ]

                SecondEnigma _ enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Bravo, voilà une première armée d'équilibrée, passons aux suivantes !"
                        , viewDiceRecap
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Utilisez les flèches pour répartir les combattants en deux armées de même valeur."
                        , viewEnigma enigmaState
                        ]

                ThirdEnigma _ enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Ce deuxième conflit ne vous a pas posé problème, montons un peu la difficulté !"
                        , viewDiceRecap
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Utilisez les flèches pour répartir les combattants en deux armées de même valeur."
                        , viewEnigma enigmaState
                        ]

                FourthEnigma _ enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Votrez habileté force le respect ! Ce quatrième conflit sera peut-être un peu plus compliqué..."
                        , viewDiceRecap
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Utilisez les flèches pour répartir les combattants en deux armées de même valeur."
                        , viewEnigma enigmaState
                        ]

                FifthEnigma _ enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Une dernière bataille vous attend, nul doute qu'une fois encore vous triompherez !"
                        , viewDiceRecap
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.M ] ] "Utilisez les flèches pour répartir les combattants en deux armées de même valeur."
                        , viewEnigma enigmaState
                        ]

                DayDone _ ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] "Vous êtes un Dieu nordique extrêment habile, félicitations !"
                        , typography HeroText p [ css [ marginTop Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 23 ]
                        , p [] [ twitterLink 23 ]
                        ]
            , homeLink
            ]


viewDiceRecap : Html msg
viewDiceRecap =
    div [ css [ textAlign center ] ]
        [ ul [ css [ display inlineBlock, textAlign left ] ]
            [ li [ class "recap-item" ]
                [ img [ src (getDieImage Soldier), class "inline-die" ] []
                , typography Paragraph span [] " 1 point"
                ]
            , li [ class "recap-item" ]
                [ img [ src (getDieImage Captain), class "inline-die" ] []
                , typography Paragraph span [] " 2 points"
                ]
            , li [ class "recap-item" ]
                [ img [ src (getDieImage Hero), class "inline-die" ] []
                , typography Paragraph span [] " 3 points"
                ]
            , li [ class "recap-item" ]
                [ img [ src (getDieImage Traitor), class "inline-die" ] []
                , typography Paragraph span [] " 1 point et annule les points d'un héros de son armée"
                ]
            , li [ class "recap-item" ]
                [ img [ src (getDieImage Cursed), class "inline-die" ] []
                , typography Paragraph span [] " -1 point"
                ]
            , li [ class "recap-item" ]
                [ img [ src (getDieImage Magus), class "inline-die" ] []
                , typography Paragraph span [] " ? = nombre de dés non mage dans leur armée"
                ]
            ]
        ]


type Group
    = FirstGroup
    | SecondGroup


viewEnigma : EnigmaState -> Html Msg
viewEnigma enigmaState =
    let
        pointsNumber score =
            case score of
                0 ->
                    "0 point"

                1 ->
                    "1 point"

                _ ->
                    String.fromInt score ++ " points"
    in
    div []
        [ viewMaybe
            (\( firstGroupScore, secondGroupScore ) ->
                typography WrongAnswer
                    p
                    [ css [ textAlign center, color Colors.secondary, marginTop Spacing.M ] ]
                    ("Raté ! Le premier groupe a " ++ pointsNumber firstGroupScore ++ " et le second groupe " ++ pointsNumber secondGroupScore ++ ". Essayez encore !")
            )
            enigmaState.lastFailedAttemptScores
        , typography Paragraph p [ css [ textAlign center, marginTop Spacing.M, marginBottom Spacing.XXS ] ] "Première armée"
        , section [ class "dice-groups" ]
            [ viewDiceGroup FirstGroup enigmaState.firstGroup
            , viewDiceGroup SecondGroup enigmaState.secondGroup
            ]
        , typography Paragraph p [ css [ textAlign center, marginTop Spacing.XXS ] ] "Deuxième armée"
        , p [ css [ textAlign center, marginTop Spacing.L ] ]
            [ button [ type_ "button", onClick ValidateAnswer ] [ text "Vérifier la combinaison" ]
            ]
        ]


viewDiceGroup : Group -> List Die -> Html Msg
viewDiceGroup group dice =
    List.sortBy dieOrder dice
        |> List.map (viewDie group)
        |> ul [ class "dice-group" ]


dieOrder : Die -> Int
dieOrder die =
    case die of
        Soldier ->
            1

        Captain ->
            2

        Hero ->
            3

        Traitor ->
            4

        BetrayedHero ->
            3

        Cursed ->
            5

        Magus ->
            6

        CalculatedMagus _ ->
            6


viewDie : Group -> Die -> Html Msg
viewDie group dice =
    li [ class "dice" ]
        [ button [ class "dice-button", onClick (MoveToGroup1 dice) ] [ text "↑" ]
            |> viewIf (group == SecondGroup)
        , img [ getDiceName dice |> alt, getDieImage dice |> src ] []
        , button [ class "dice-button", onClick (MoveToGroup2 dice) ] [ text "↓" ]
            |> viewIf (group == FirstGroup)
        ]


getDieImage : Die -> String
getDieImage dice =
    case dice of
        Soldier ->
            ImagePath.toString images.day18.soldier

        Captain ->
            ImagePath.toString images.day18.captain

        Hero ->
            ImagePath.toString images.day18.hero

        Traitor ->
            ImagePath.toString images.day18.traitor

        BetrayedHero ->
            ImagePath.toString images.day18.hero

        Cursed ->
            ImagePath.toString images.day18.cursed

        Magus ->
            ImagePath.toString images.day18.magus

        CalculatedMagus _ ->
            ImagePath.toString images.day18.magus


getDiceName : Die -> String
getDiceName dice =
    case dice of
        Soldier ->
            "Soldat"

        Captain ->
            "Capitaine"

        Hero ->
            "Héros"

        Traitor ->
            "Traître"

        BetrayedHero ->
            "Héros trahis"

        Cursed ->
            "Maudit"

        Magus ->
            "Mage"

        CalculatedMagus _ ->
            "Mage calculé"


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

        ThirdEnigma firstEnigmasScore enigmaState ->
            Encode.object
                [ ( "state", Encode.string "third-enigma" )
                , ( "first-enigmas-score", Encode.int firstEnigmasScore )
                , ( "enigma-state", encodeEnigmaState enigmaState )
                ]

        FourthEnigma firstEnigmasScore enigmaState ->
            Encode.object
                [ ( "state", Encode.string "fourth-enigma" )
                , ( "first-enigmas-score", Encode.int firstEnigmasScore )
                , ( "enigma-state", encodeEnigmaState enigmaState )
                ]

        FifthEnigma firstEnigmasScore enigmaState ->
            Encode.object
                [ ( "state", Encode.string "fifth-enigma" )
                , ( "first-enigmas-score", Encode.int firstEnigmasScore )
                , ( "enigma-state", encodeEnigmaState enigmaState )
                ]

        DayDone score ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "score", Encode.int score )
                ]


encodeEnigmaState : EnigmaState -> Encode.Value
encodeEnigmaState enigmaState =
    Encode.object
        [ ( "failed-attempts", Encode.int enigmaState.failedAttempts ) ]


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
                        Decode.map2 ThirdEnigma
                            (Decode.field "first-enigmas-score" Decode.int)
                            (Decode.field "enigma-state" (enigmaStateDecoder thirdEnigma))

                    "fourth-enigma" ->
                        Decode.map2 FourthEnigma
                            (Decode.field "first-enigmas-score" Decode.int)
                            (Decode.field "enigma-state" (enigmaStateDecoder fourthEnigma))

                    "fifth-enigma" ->
                        Decode.map2 FifthEnigma
                            (Decode.field "first-enigmas-score" Decode.int)
                            (Decode.field "enigma-state" (enigmaStateDecoder fifthEnigma))

                    "done" ->
                        Decode.map DayDone (Decode.field "score" Decode.int)

                    _ ->
                        Decode.fail "Invalid state"
            )


enigmaStateDecoder : Enigma -> Decoder EnigmaState
enigmaStateDecoder enigma =
    Decode.map
        (\failedAttempts ->
            { firstGroup = List.take 4 enigma.warriors
            , secondGroup = List.drop 4 enigma.warriors
            , failedAttempts = failedAttempts
            , lastFailedAttemptScores = Nothing
            }
        )
        (Decode.field "failed-attempts" Decode.int)


styles : List Snippet
styles =
    [ Css.class "dice-groups"
        []
    , Css.class "dice-group"
        [ displayFlex
        , justifyContent center
        ]
    , Css.class "dice"
        [ displayFlex
        , flexDirection column
        , alignItems center
        , marginLeft Spacing.XS
        , marginRight Spacing.XS
        , Css.children
            [ Css.img
                [ height (px 30)
                , width (px 30)
                ]
            , Css.button
                [ backgroundColor transparent
                , borderColor transparent
                , color (rgb 0 0 0)
                , fontSize (em 1.5)
                , fontWeight bold
                , paddingTop Spacing.XXS
                , paddingBottom Spacing.XXS
                , Css.hover [ backgroundColor (rgb 230 230 230) ]
                , Css.focus [ backgroundColor (rgb 210 210 210) ]
                ]
            ]
        ]
    , Css.class "recap-item"
        [ displayFlex
        , alignItems center
        , marginTop Spacing.S
        ]
    , Css.class "inline-die"
        [ marginLeft Spacing.XXS
        , marginRight Spacing.XXS
        , height (em 1.5)
        , width (em 1.5)
        ]
    ]
