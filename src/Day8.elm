module Day8 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (absolute, alignItems, alignSelf, backgroundColor, batch, bold, borderRadius, center, color, column, cursor, flexDirection, fontWeight, height, justifyContent, left, opacity, pct, pointer, position, relative, row, spaceBetween, textAlign, top, width)
import DesignSystem.Colors exposing (green, red, white)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, h1, img, p, text)
import Html.Styled.Attributes exposing (alt, class, css, src, type_)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages exposing (PathKey, images)
import Pages.ImagePath as ImagePath exposing (ImagePath)
import Random
import Random.Extra
import Random.List
import Time exposing (Posix, Zone)
import Vector30 as Symbol
import Vector7 exposing (Vector7)
import Vector8 exposing (Vector8)


type Model
    = ShowInstructionsOnly
    | InProgress Game
    | Done { success : Int, fails : Int, elapsedTime : Int, previousAnswer : Maybe ( Time.Posix, Answer ) }


type alias Symbol =
    Symbol.Index


type alias Symbols =
    Symbol.Vector30 Symbol


allSymbols : Symbols
allSymbols =
    Symbol.indices


shuffledSymbolsGenerator : Random.Generator Symbols
shuffledSymbolsGenerator =
    Random.List.shuffle (Symbol.toList allSymbols)
        |> Random.map
            (\shuffledList ->
                Symbol.fromListWithDefault Symbol.Index0 shuffledList
                    |> Tuple.second
            )


type alias Pair =
    { commonSymbol : Symbol
    , firstCard : Vector7 Symbol
    , secondCard : Vector7 Symbol
    }


pairGenerator : Random.Generator Pair
pairGenerator =
    shuffledSymbolsGenerator
        |> Random.map
            (\shuffled ->
                { commonSymbol = Symbol.get Symbol.Index0 shuffled
                , firstCard =
                    Vector7.from7
                        (Symbol.get Symbol.Index1 shuffled)
                        (Symbol.get Symbol.Index2 shuffled)
                        (Symbol.get Symbol.Index3 shuffled)
                        (Symbol.get Symbol.Index4 shuffled)
                        (Symbol.get Symbol.Index5 shuffled)
                        (Symbol.get Symbol.Index6 shuffled)
                        (Symbol.get Symbol.Index7 shuffled)
                , secondCard =
                    Vector7.from7
                        (Symbol.get Symbol.Index8 shuffled)
                        (Symbol.get Symbol.Index9 shuffled)
                        (Symbol.get Symbol.Index10 shuffled)
                        (Symbol.get Symbol.Index11 shuffled)
                        (Symbol.get Symbol.Index12 shuffled)
                        (Symbol.get Symbol.Index13 shuffled)
                        (Symbol.get Symbol.Index14 shuffled)
                }
            )


type alias Card =
    Vector8 ( Answer, Symbol )


toCards : Pair -> ( Card, Card )
toCards pair =
    ( Vector7.map (Tuple.pair Loose) pair.firstCard
        |> Vector7.push ( Win, pair.commonSymbol )
    , Vector7.map (Tuple.pair Loose) pair.secondCard
        |> Vector7.push ( Win, pair.commonSymbol )
    )


type alias DrawedSymbol =
    { symbol : Symbol
    , rotationDeg : Float
    , scale : Float
    , answer : Answer
    }


type Answer
    = Win
    | Loose


drawedSymbolGenerator : ( Answer, Symbol ) -> Random.Generator DrawedSymbol
drawedSymbolGenerator ( answer, symbol ) =
    Random.map2
        (\rotationDeg scale ->
            { symbol = symbol
            , answer = answer
            , rotationDeg = rotationDeg
            , scale = scale
            }
        )
        (Random.float 0 360)
        (Random.float 0.5 0.1)


type alias DrawedCard =
    { symbols : Vector8 DrawedSymbol
    , rotationDeg : Float
    }


drawedCardGenerator : Card -> Random.Generator DrawedCard
drawedCardGenerator card =
    let
        emptyDrawedSymbol =
            DrawedSymbol Symbol.Index0 0 0 Loose
    in
    Random.map2 DrawedCard
        (Random.Extra.traverse drawedSymbolGenerator (Vector8.toList card)
            |> Random.andThen
                (\drawedCards ->
                    Random.List.shuffle drawedCards
                        |> Random.map
                            (\shuffled ->
                                Vector8.fromListWithDefault emptyDrawedSymbol shuffled
                                    |> Tuple.second
                            )
                )
        )
        (Random.float 0 360)


type alias DrawedPair =
    ( DrawedCard, DrawedCard )


drawedPairGenerator : Random.Generator DrawedPair
drawedPairGenerator =
    pairGenerator
        |> Random.map toCards
        |> Random.andThen
            (\( card1, card2 ) ->
                Random.pair
                    (drawedCardGenerator card1)
                    (drawedCardGenerator card2)
            )


getDrawedPair : Int -> DrawedPair
getDrawedPair i =
    Random.step drawedPairGenerator (Random.initialSeed i)
        |> Tuple.first


type alias Game =
    { currentPair : DrawedPair
    , remainingPairs : List DrawedPair
    , success : Int
    , fails : Int
    , startTime : Time.Posix
    , previousAnswer : Maybe ( Time.Posix, Answer )
    }


init : Model
init =
    ShowInstructionsOnly


type Msg
    = ClickedStart
    | ClickedCard Answer


update : Time.Posix -> Model -> Msg -> Model
update now model msg =
    case ( model, msg ) of
        ( ShowInstructionsOnly, ClickedStart ) ->
            let
                seed =
                    Time.posixToMillis now
            in
            InProgress
                { currentPair = getDrawedPair seed
                , remainingPairs =
                    List.map getDrawedPair (List.range (seed + 1) (seed + 11))
                , startTime = now
                , success = 0
                , fails = 0
                , previousAnswer = Nothing
                }

        ( InProgress game, ClickedCard answer ) ->
            let
                updatedScore =
                    case answer of
                        Win ->
                            { game | success = game.success + 1 }

                        Loose ->
                            { game | fails = game.fails + 1 }
            in
            case game.remainingPairs of
                [] ->
                    Done
                        { success = updatedScore.success
                        , fails = updatedScore.fails
                        , elapsedTime = Time.posixToMillis now - Time.posixToMillis updatedScore.startTime
                        , previousAnswer = Just ( now, answer )
                        }

                first :: others ->
                    InProgress { updatedScore | currentPair = first, remainingPairs = others, previousAnswer = Just ( now, answer ) }

        _ ->
            -- impossible transition!
            model


view : Zone -> Posix -> Model -> Html Msg
view zone now model =
    let
        maxDay =
            if Time.toYear zone now > 2020 then
                24

            else
                Time.toDay zone now
    in
    if maxDay < 8 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day8" ]
            [ typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 8 | Dobble"
            , case model of
                Done previousResults ->
                    div [ css [ textAlign center, marginTop Spacing.XL, Css.displayFlex, flexDirection column ] ]
                        [ viewPreviousAnswer now previousResults.previousAnswer
                        , typography HeroText
                            p
                            []
                            ("Défi terminé ! Vous avez mis "
                                ++ durationToStr (previousResults.elapsedTime // 1000)
                                ++ " pour trouver "
                                ++ String.fromInt previousResults.success
                                ++ " symboles "
                                ++ (case previousResults.fails of
                                        0 ->
                                            "en ne commettant aucune erreur !"

                                        1 ->
                                            "en commettant une seule erreur !"

                                        n ->
                                            "en commettant " ++ String.fromInt n ++ " erreurs !"
                                   )
                            )
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 6 ]
                        , p [] [ twitterLink 6 ]
                        ]

                ShowInstructionsOnly ->
                    div [ css [ Css.displayFlex, flexDirection column, justifyContent spaceBetween, alignItems center ] ]
                        [ instructions
                        , img [ src (ImagePath.toString images.day8.explanations), css [ width (pct 100) ], alt "exemple d'images" ] []
                        , button
                            [ onClick ClickedStart, type_ "button", css [ cursor pointer ] ]
                            [ text "Commencer !" ]
                        ]

                InProgress game ->
                    let
                        ( first, second ) =
                            game.currentPair
                    in
                    div [ css [ Css.displayFlex, flexDirection column, justifyContent spaceBetween, alignItems Css.stretch ] ]
                        [ instructions
                        , viewPreviousAnswer now game.previousAnswer
                        , div
                            [ css
                                [ Css.displayFlex
                                , flexDirection row
                                , justifyContent spaceBetween
                                , Spacing.marginTop Spacing.S
                                , Css.flexFlow1 Css.wrap
                                ]
                            ]
                            [ viewDrawedCard first
                            , viewDrawedCard second
                            ]
                        ]
            ]


durationToStr : Int -> String
durationToStr duration =
    if duration < 60 then
        String.fromInt duration ++ " secondes"

    else if duration < 120 then
        String.fromInt (duration // 60) ++ " minute et " ++ String.fromInt (remainderBy 60 duration) ++ " secondes"

    else
        String.fromInt (duration // 60) ++ " minutes et " ++ String.fromInt (remainderBy 60 duration) ++ " secondes"


viewPreviousAnswer : Posix -> Maybe ( Posix, Answer ) -> Html msg
viewPreviousAnswer now previousAnswer =
    case previousAnswer of
        Just ( clickedAt, answer ) ->
            div
                [ css
                    [ alignSelf Css.end
                    , Spacing.padding Spacing.XS
                    , borderRadius (Css.px 3)
                    , color white
                    , if Time.posixToMillis now - Time.posixToMillis clickedAt < 2 then
                        batch []

                      else
                        batch
                            [ opacity (Css.int 0)
                            , Css.property "transition" "opacity 1s ease-out"
                            ]
                    , case answer of
                        Win ->
                            batch
                                [ backgroundColor green
                                , fontWeight bold
                                ]

                        Loose ->
                            batch
                                [ backgroundColor red
                                , fontWeight bold
                                ]
                    ]
                ]
                [ text <|
                    case answer of
                        Win ->
                            "OK"

                        Loose ->
                            "NO"
                ]

        Nothing ->
            text ""


instructions : Html Msg
instructions =
    typography Instructions p [ css [ textAlign center ] ] "Cliquez sur le symbole en commun entre les deux cartes !"


cardDiam : Float
cardDiam =
    22


symbolDiam : Float
symbolDiam =
    cardDiam / 3


toTopLeft : ( Float, Float ) -> ( Float, Float )
toTopLeft ( x, y ) =
    ( x + cardDiam / 2 - symbolDiam / 2 + 1, y + cardDiam / 2 - symbolDiam / 2 )


fromTheta : Float -> ( Float, Float )
fromTheta theta =
    ( 0.9 * symbolDiam * cos (degrees theta), 0.9 * symbolDiam * sin (degrees theta) )
        |> toTopLeft


viewDrawedCard : DrawedCard -> Html Msg
viewDrawedCard drawedCard =
    div
        [ css
            [ borderRadius (Css.px 99999)
            , Css.property "box-shadow" "0 10px 8px 0 rgba(0, 0, 0, .18), 0 5px 5px 0 rgba(0, 0, 0, .15)"
            ]
        ]
        [ div
            [ css
                [ borderRadius (Css.px 99999)
                , width (Css.rem cardDiam)
                , height (Css.rem cardDiam)
                , backgroundColor (Css.hex "FFF")
                , position relative
                , Spacing.marginTop Spacing.S
                , Css.transform (Css.rotate (Css.deg drawedCard.rotationDeg))
                ]
            ]
            [ viewDrawedSymbol (toTopLeft ( 0, 0 )) (Vector8.get Vector8.Index0 drawedCard.symbols)
            , viewDrawedSymbol (fromTheta 0) (Vector8.get Vector8.Index1 drawedCard.symbols)
            , viewDrawedSymbol (fromTheta (1 / 7 * 360)) (Vector8.get Vector8.Index2 drawedCard.symbols)
            , viewDrawedSymbol (fromTheta (2 / 7 * 360)) (Vector8.get Vector8.Index3 drawedCard.symbols)
            , viewDrawedSymbol (fromTheta (3 / 7 * 360)) (Vector8.get Vector8.Index4 drawedCard.symbols)
            , viewDrawedSymbol (fromTheta (4 / 7 * 360)) (Vector8.get Vector8.Index5 drawedCard.symbols)
            , viewDrawedSymbol (fromTheta (5 / 7 * 360)) (Vector8.get Vector8.Index6 drawedCard.symbols)
            , viewDrawedSymbol (fromTheta (6 / 7 * 360)) (Vector8.get Vector8.Index7 drawedCard.symbols)
            ]
        ]


viewDrawedSymbol : ( Float, Float ) -> DrawedSymbol -> Html Msg
viewDrawedSymbol ( x, y ) drawedSymbol =
    img
        [ css
            [ position absolute
            , top (Css.rem y)
            , left (Css.rem x)
            , width (Css.rem (symbolDiam * drawedSymbol.scale))
            , height (Css.rem (symbolDiam * drawedSymbol.scale))
            , Css.transform (Css.rotate (Css.deg drawedSymbol.rotationDeg))
            , Css.cursor Css.pointer
            ]
        , onClick (ClickedCard drawedSymbol.answer)
        , src (ImagePath.toString (toImage drawedSymbol.symbol))
        ]
        []


toImage : Symbol -> ImagePath PathKey
toImage symbol =
    case symbol of
        Symbol.Index1 ->
            images.day8.dobble1

        Symbol.Index10 ->
            images.day8.dobble10

        Symbol.Index11 ->
            images.day8.dobble11

        Symbol.Index12 ->
            images.day8.dobble12

        Symbol.Index13 ->
            images.day8.dobble13

        Symbol.Index14 ->
            images.day8.dobble14

        Symbol.Index15 ->
            images.day8.dobble15

        Symbol.Index16 ->
            images.day8.dobble16

        Symbol.Index17 ->
            images.day8.dobble17

        Symbol.Index18 ->
            images.day8.dobble18

        Symbol.Index19 ->
            images.day8.dobble19

        Symbol.Index2 ->
            images.day8.dobble2

        Symbol.Index20 ->
            images.day8.dobble20

        Symbol.Index21 ->
            images.day8.dobble21

        Symbol.Index22 ->
            images.day8.dobble22

        Symbol.Index23 ->
            images.day8.dobble23

        Symbol.Index24 ->
            images.day8.dobble24

        Symbol.Index25 ->
            images.day8.dobble25

        Symbol.Index26 ->
            images.day8.dobble26

        Symbol.Index27 ->
            images.day8.dobble27

        Symbol.Index28 ->
            images.day8.dobble28

        Symbol.Index29 ->
            images.day8.dobble29

        Symbol.Index3 ->
            images.day8.dobble3

        Symbol.Index0 ->
            images.day8.dobble30

        Symbol.Index4 ->
            images.day8.dobble4

        Symbol.Index5 ->
            images.day8.dobble5

        Symbol.Index6 ->
            images.day8.dobble6

        Symbol.Index7 ->
            images.day8.dobble7

        Symbol.Index8 ->
            images.day8.dobble8

        Symbol.Index9 ->
            images.day8.dobble9


isDone : Model -> Bool
isDone model =
    case model of
        Done _ ->
            True

        _ ->
            False


saveState : Model -> Encode.Value
saveState model =
    case model of
        Done previousResults ->
            Encode.object
                [ ( "success", Encode.int previousResults.success )
                , ( "fails", Encode.int previousResults.fails )
                , ( "elapsedTime", Encode.int previousResults.elapsedTime )
                ]

        InProgress _ ->
            Encode.string "init"

        ShowInstructionsOnly ->
            Encode.string "init"


stateDecoder : Decoder Model
stateDecoder =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "init" ->
                            Decode.succeed ShowInstructionsOnly

                        _ ->
                            Decode.fail ("unknown state: " ++ s)
                )
        , Decode.map3
            (\success fails elapsedTime -> Done { success = success, fails = fails, elapsedTime = elapsedTime, previousAnswer = Nothing })
            (Decode.field "success" Decode.int)
            (Decode.field "fails" Decode.int)
            (Decode.field "elapsedTime" Decode.int)
        ]
