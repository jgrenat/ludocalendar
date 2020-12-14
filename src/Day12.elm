module Day12 exposing (Model, Msg, init, isDone, saveState, stateDecoder, subscriptions, update, view)

import Css exposing (absolute, alignItems, backgroundImage, backgroundRepeat, before, bold, center, content, deg, displayFlex, flexWrap, fontWeight, height, hidden, int, justifyContent, left, overflow, pct, position, relative, rem, repeat, rotate, textAlign, top, transform, url, width, wrap, zIndex)
import Css.Global as Css
import DesignSystem.Colors as Colors
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, br, button, div, form, h1, img, input, li, p, span, text, ul)
import Html.Styled.Attributes exposing (class, classList, css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages exposing (PathKey, images)
import Pages.ImagePath exposing (ImagePath)
import Random exposing (Generator)
import Random.List
import Time exposing (Posix, Zone)
import Vector18 as VectorSymbol
import Vector36 as VectorBoard



------------
-- SYMBOL
------------


type alias Symbol =
    VectorSymbol.Index


symbols : List Symbol
symbols =
    VectorSymbol.indices
        |> VectorSymbol.toList


symbolSize : Css.Rem
symbolSize =
    rem 5


toImagePath : Symbol -> ImagePath PathKey
toImagePath symbol =
    case symbol of
        VectorSymbol.Index0 ->
            images.day8.dobble18

        VectorSymbol.Index1 ->
            images.day8.dobble1

        VectorSymbol.Index2 ->
            images.day8.dobble2

        VectorSymbol.Index3 ->
            images.day8.dobble3

        VectorSymbol.Index4 ->
            images.day8.dobble4

        VectorSymbol.Index5 ->
            images.day8.dobble5

        VectorSymbol.Index6 ->
            images.day8.dobble6

        VectorSymbol.Index7 ->
            images.day8.dobble7

        VectorSymbol.Index8 ->
            images.day8.dobble8

        VectorSymbol.Index9 ->
            images.day8.dobble9

        VectorSymbol.Index10 ->
            images.day8.dobble10

        VectorSymbol.Index11 ->
            images.day8.dobble11

        VectorSymbol.Index12 ->
            images.day8.dobble12

        VectorSymbol.Index13 ->
            images.day8.dobble13

        VectorSymbol.Index14 ->
            images.day8.dobble14

        VectorSymbol.Index15 ->
            images.day8.dobble15

        VectorSymbol.Index16 ->
            images.day8.dobble16

        VectorSymbol.Index17 ->
            images.day8.dobble17


viewSymbol : Symbol -> Html Msg
viewSymbol symbol =
    img
        [ css
            [ width symbolSize
            , height symbolSize
            ]
        , src (Pages.ImagePath.toString (toImagePath symbol))
        ]
        []



------------
-- CARD
-----------


type Card
    = Card Symbol
    | Removed


cardSize : Css.Rem
cardSize =
    rem 7


viewCard : Picking -> ( VectorBoard.Index, Card ) -> Html Msg
viewCard picking ( index, card ) =
    case card of
        Card symbol ->
            if isPicked index picking then
                div
                    [ css
                        [ width cardSize
                        , height cardSize
                        , Css.property "box-shadow" "0 10px 8px 0 rgba(0, 0, 0, .18), 0 5px 5px 0 rgba(0, 0, 0, .15)"
                        , Spacing.marginLeft Spacing.S
                        , Spacing.marginTop Spacing.S
                        , displayFlex
                        , justifyContent center
                        , alignItems center
                        ]
                    ]
                    [ viewSymbol symbol
                    ]

            else
                div
                    [ css
                        [ width cardSize
                        , height cardSize
                        , Css.property "box-shadow" "0 10px 8px 0 rgba(0, 0, 0, .18), 0 5px 5px 0 rgba(0, 0, 0, .15)"
                        , Spacing.marginLeft Spacing.S
                        , Spacing.marginTop Spacing.S
                        , position relative
                        , overflow hidden
                        , before
                            [ backgroundImage (url (Pages.ImagePath.toString images.day12.cardBackground))
                            , backgroundRepeat repeat
                            , Css.property "content" "\"\""
                            , position absolute
                            , width (pct 200)
                            , height (pct 200)
                            , top (pct -50)
                            , left (pct -50)

                            -- , zIndex (int -1)
                            , transform (rotate (deg 45))
                            ]
                        ]
                    , if canPickAnother picking then
                        onClick (Picked ( index, symbol ))

                      else
                        class ""
                    ]
                    []

        Removed ->
            div
                [ css
                    [ width cardSize
                    , height cardSize
                    , Spacing.marginLeft Spacing.S
                    , Spacing.marginTop Spacing.S
                    ]
                ]
                []


encodeCard : Card -> Encode.Value
encodeCard card =
    case card of
        Removed ->
            Encode.object [ ( "kind", Encode.string "removed" ) ]

        Card symbol ->
            Encode.object [ ( "kind", Encode.string "card" ), ( "symbol", Encode.int (VectorSymbol.indexToInt symbol) ) ]


cardDecoder : Decode.Decoder Card
cardDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "removed" ->
                        Decode.succeed Removed

                    "card" ->
                        Decode.field "symbol" Decode.int
                            |> Decode.andThen
                                (\i ->
                                    case VectorSymbol.intToIndex i of
                                        Nothing ->
                                            Decode.fail ("wrong index: " ++ String.fromInt i)

                                        Just index ->
                                            Decode.succeed (Card index)
                                )

                    _ ->
                        Decode.fail ("unknown kind: " ++ s)
            )



------------
-- BOARD
------------


type alias VectorBoard a =
    VectorBoard.Vector36 a


type alias Board =
    VectorBoard Card


boardGenerator : Generator Board
boardGenerator =
    (symbols ++ symbols)
        |> List.map Card
        |> Random.List.shuffle
        |> Random.map
            (\shuffled ->
                VectorBoard.fromListWithDefault Removed shuffled
                    |> Tuple.second
            )


viewBoard : Picking -> Board -> Html Msg
viewBoard picking board =
    div [ css [ displayFlex, flexWrap wrap ] ]
        (VectorBoard.toIndexedList board
            |> List.map (viewCard picking)
        )



-------------
-- PICKING
-------------


type Picking
    = NoPicking
    | OneCard ( VectorBoard.Index, Symbol )
    | TwoCards ( VectorBoard.Index, Symbol ) ( VectorBoard.Index, Symbol )


isPicked : VectorBoard.Index -> Picking -> Bool
isPicked index picking =
    case picking of
        NoPicking ->
            False

        OneCard ( i, _ ) ->
            i == index

        TwoCards ( i1, _ ) ( i2, _ ) ->
            i1 == index || i2 == index


canPickAnother : Picking -> Bool
canPickAnother picking =
    case picking of
        NoPicking ->
            True

        OneCard _ ->
            True

        TwoCards _ _ ->
            False



----------
-- GAME
----------


type alias Game =
    { board : Board, fails : Int, picking : Picking }


type Model
    = InProgress Game
    | Done { fails : Int }


type Msg
    = Picked ( VectorBoard.Index, Symbol )
    | DelayElapsedAfterPairSelected


update : Model -> Msg -> Model
update model msg =
    case model of
        Done _ ->
            model

        InProgress game ->
            case msg of
                Picked indexedSymbol ->
                    InProgress
                        { game
                            | picking =
                                case game.picking of
                                    NoPicking ->
                                        OneCard indexedSymbol

                                    OneCard first ->
                                        TwoCards first indexedSymbol

                                    TwoCards _ _ ->
                                        -- the player shouldn't be able to select
                                        -- a third card if he already had selected two
                                        -- of them.
                                        game.picking
                        }

                DelayElapsedAfterPairSelected ->
                    case game.picking of
                        NoPicking ->
                            -- should not happen
                            model

                        OneCard _ ->
                            -- should not happen
                            model

                        TwoCards ( index1, symbol1 ) ( index2, symbol2 ) ->
                            if symbol1 == symbol2 then
                                let
                                    newBoard =
                                        game.board
                                            |> VectorBoard.set index1 Removed
                                            |> VectorBoard.set index2 Removed
                                in
                                if List.all ((==) Removed) (VectorBoard.toList newBoard) then
                                    Done { fails = game.fails }

                                else
                                    InProgress { game | board = newBoard, picking = NoPicking }

                            else
                                InProgress
                                    { game
                                        | fails = game.fails + 1
                                        , picking = NoPicking
                                    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        InProgress game ->
            case game.picking of
                NoPicking ->
                    Sub.none

                OneCard _ ->
                    Sub.none

                TwoCards _ _ ->
                    Time.every 750 (always DelayElapsedAfterPairSelected)

        Done _ ->
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
    if maxDay < 12 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 12 | Memory"
            , case state of
                InProgress game ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Cliquez sur les cartes pour associer les paires !"
                        , viewBoard game.picking game.board
                        ]

                Done data ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [ css [ marginTop Spacing.M ] ] ("Bravo ! Vous avez trouvé toutes les paires en ne vous trompant que " ++ String.fromInt data.fails ++ " fois !")
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 12 ]
                        , p [] [ twitterLink 12 ]
                        ]
            , typography Paragraph p [ css [ marginTop Spacing.L, fontWeight bold, textAlign center ] ] "Encore une fois, un énorme merci à Seb & Laure qui ont créé également l'épreuve d'aujourd'hui ! 🎉"
            , homeLink
            ]


init : Model
init =
    InProgress
        { board = Random.step boardGenerator (Random.initialSeed 1) |> Tuple.first
        , fails = 0
        , picking = NoPicking
        }


isDone : Model -> Bool
isDone model =
    case model of
        Done _ ->
            True

        InProgress _ ->
            False


saveState : Model -> Encode.Value
saveState model =
    case model of
        Done data ->
            Encode.object [ ( "state", Encode.string "done" ), ( "fails", Encode.int data.fails ) ]

        InProgress game ->
            Encode.object
                [ ( "state", Encode.string "inProgress" )
                , ( "fails", Encode.int game.fails )
                , ( "board", Encode.list encodeCard (VectorBoard.toList game.board) )
                ]


stateDecoder : Decode.Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "done" ->
                        Decode.field "fails" Decode.int
                            |> Decode.map (\fails -> Done { fails = fails })

                    "inProgress" ->
                        Decode.map3 Game
                            (Decode.field "board" (Decode.list cardDecoder)
                                |> Decode.andThen
                                    (\cards ->
                                        case VectorBoard.fromList cards of
                                            Just ( [], board ) ->
                                                Decode.succeed board

                                            Just _ ->
                                                Decode.fail "too much cards!"

                                            Nothing ->
                                                Decode.fail "not enough cards!"
                                    )
                            )
                            (Decode.field "fails" Decode.int)
                            (Decode.succeed NoPicking)
                            |> Decode.map InProgress

                    _ ->
                        Decode.fail ("unknown state: " ++ s)
            )
