module Day22 exposing (Model, Msg, init, isDone, saveState, stateDecoder, subscriptions, update, view)

import Browser
import Css exposing (alignItems, center, color, column, cursor, displayFlex, flexDirection, flexShrink, flexWrap, hex, justifyContent, pointer, right, row, spaceBetween, textAlign, textTransform, uppercase, wrap, zIndex)
import Css.Global as Css exposing (Snippet)
import Css.Transitions exposing (easeInOut, transition)
import DesignSystem.Colors as Colors exposing (green, white)
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html exposing (Attribute)
import Html.Styled exposing (Html, br, button, div, form, h1, input, li, p, span, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (attribute, class, classList, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import List.Extra
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Random
import Random.List
import String.Extra as String
import String.Normalize exposing (removeDiacritics)
import Svg.Styled exposing (defs, path, pattern, rect, svg)
import Svg.Styled.Attributes exposing (d, fill, height, id, ry, stroke, strokeWidth, viewBox, width)
import Time exposing (Posix, Zone)


type alias Card =
    { shape : Shape
    , count : Count
    , fill : Fill
    , color : Color
    }


type Shape
    = Diamond
    | Rectangle
    | Wave


toPatternId : Color -> String
toPatternId color =
    "pattern-"
        ++ (case color of
                Red ->
                    "red"

                Purple ->
                    "purple"

                Green ->
                    "green"
           )


viewShape : Shape -> Fill -> Color -> Html msg
viewShape shape fillArg color =
    let
        strokeAttrs =
            [ stroke (toHtmlColor color)
            , strokeWidth "2"
            ]

        ( fillDefs, fillAttrs ) =
            case fillArg of
                FillEmpty ->
                    ( defs [] [], [ fill "none" ] )

                FillPlain ->
                    ( defs [] [], [ fill (toHtmlColor color) ] )

                FillStripped ->
                    ( defs []
                        [ pattern [ id (toPatternId color), width "2", height "100%", attribute "patternUnits" "userSpaceOnUse" ]
                            [ rect [ fill (toHtmlColor color), width "0.666", height "100%" ] [] ]
                        ]
                    , [ fill ("url(#" ++ toPatternId color ++ ")") ]
                    )

        commonAttrs =
            fillAttrs ++ strokeAttrs
    in
    case shape of
        Diamond ->
            svg [ viewBox "-2 -2 60 32" ]
                [ fillDefs
                , path
                    (commonAttrs
                        ++ [ d "m 0,14 28,-14 28,14 -28,14 z" ]
                    )
                    []
                ]

        Rectangle ->
            svg [ viewBox "-1 -1 56.2 27.4" ]
                [ fillDefs
                , rect
                    (commonAttrs
                        ++ [ width "54.2", height "25.4", ry "12.7" ]
                    )
                    []
                ]

        Wave ->
            svg [ viewBox "204.1 260.6 52.35 27.95" ]
                [ fillDefs
                , path
                    (commonAttrs
                        ++ [ d "m 205.1026,277.45527 c -0.13364,-19.10978 17.50615,-16.8513 31.13689,-10.42351 12.82894,4.14267 17.47399,-14.55089 19.10979,0.80179 0.75173,7.05535 -6.41448,21.24793 -23.11883,14.96711 -15.36799,-8.8199 -20.31248,7.88446 -25.65787,2.80632 -1.60362,-3.0736 -1.46998,-8.15171 -1.46998,-8.15171 z" ]
                    )
                    []
                ]


viewCard : Picking -> Card -> Html GameMsg
viewCard picking card =
    let
        shape =
            viewShape card.shape card.fill card.color

        selected =
            case picking of
                NoPicking ->
                    False

                PickingOne c ->
                    c == card

                PickingTwo ( c1, c2 ) ->
                    c1 == card || c2 == card

                PickingThree ( c1, c2, c3 ) ->
                    c1 == card || c2 == card || c3 == card
    in
    div
        [ css
            [ Css.borderRadius (Css.px 10)
            , Css.width (Css.px 100)
            , Css.height (Css.px 167)
            , Css.padding (Css.px 20)
            , Css.margin (Css.px 12)
            , displayFlex
            , Css.backgroundColor white
            , flexDirection column
            , alignItems center
            , Css.property "justify-content" "space-evenly"
            , transition [ Css.Transitions.transform 300, Css.Transitions.boxShadow 300 ]
            , cursor pointer
            ]
        , if selected then
            css
                [ Css.transform (Css.translate2 (Css.px 10) (Css.px 10))
                , Css.property "box-shadow" "0 12px 10px 0 rgba(0, 70, 32, 0.50), 0 7px 7px 0 rgba(0, 70, 32, 0.50)"
                ]

          else
            css [ Css.property "box-shadow" "0 10px 8px 0 rgba(0, 0, 0, .18), 0 5px 5px 0 rgba(0, 0, 0, .15)" ]
        , onClick (Picked card)
        ]
        (case card.count of
            One ->
                [ shape ]

            Two ->
                [ shape, shape ]

            Three ->
                [ shape, shape, shape ]
        )


viewBoard : Picking -> List Card -> Html GameMsg
viewBoard picking board =
    div
        [ css
            [ displayFlex
            , flexDirection row
            , flexWrap wrap
            , flexShrink (Css.int 1)
            , Spacing.marginTop Spacing.S
            ]
        ]
        (List.map (viewCard picking) board)


shapes : List Shape
shapes =
    [ Diamond, Rectangle, Wave ]


type Count
    = One
    | Two
    | Three


counts : List Count
counts =
    [ One, Two, Three ]


type Fill
    = FillEmpty
    | FillPlain
    | FillStripped


fills : List Fill
fills =
    [ FillEmpty, FillPlain, FillStripped ]


type Color
    = Green
    | Red
    | Purple


toHtmlColor : Color -> String
toHtmlColor color =
    case color of
        Purple ->
            "#4a347f"

        Green ->
            "#029e30"

        Red ->
            "#e1173f"


colors : List Color
colors =
    [ Green, Red, Purple ]


cards : List Card
cards =
    shapes
        |> List.concatMap
            (\shape ->
                counts
                    |> List.concatMap
                        (\count ->
                            fills
                                |> List.concatMap
                                    (\fill ->
                                        colors
                                            |> List.concatMap
                                                (\color ->
                                                    [ Card shape count fill color ]
                                                )
                                    )
                        )
            )


type alias T3 a =
    ( a, a, a )


map3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
map3 f ( a, b, c ) =
    ( f a, f b, f c )


validFeatures : T3 feature -> Bool
validFeatures ( f1, f2, f3 ) =
    (f1 /= f2 && f2 /= f3 && f3 /= f1)
        || (f1 == f2 && f2 == f3)


isSet : T3 Card -> Bool
isSet triple =
    validFeatures (map3 .shape triple)
        && validFeatures (map3 .count triple)
        && validFeatures (map3 .fill triple)
        && validFeatures (map3 .color triple)


push2 : a -> b -> ( a, b )
push2 =
    Tuple.pair


pairs : List a -> List ( a, a )
pairs xs =
    case xs of
        [] ->
            []

        first :: others ->
            List.map (push2 first) others
                ++ pairs others


push3 : a -> ( b, c ) -> ( a, b, c )
push3 a ( b, c ) =
    ( a, b, c )


triples : List a -> List ( a, a, a )
triples xs =
    case xs of
        [] ->
            []

        first :: others ->
            List.map (push3 first) (pairs others)
                ++ triples others


containsSet : List Card -> Bool
containsSet board =
    triples board
        |> List.any isSet


type Model
    = ReadInstruction
    | InProgress Game
    | Done DoneData


type alias DoneData =
    { validTriples : Int
    , mistakes : Int
    }


init : Model
init =
    ReadInstruction


type Msg
    = GameMsg GameMsg
    | ClickedStart


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedStart ->
            InProgress (gameInit (Random.initialSeed 1))

        GameMsg subMsg ->
            case model of
                ReadInstruction ->
                    model

                InProgress game ->
                    updateGame subMsg game

                Done data ->
                    model


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate model =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay < 22 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 22 | Set"
            , case model of
                ReadInstruction ->
                    div [ css [ Css.displayFlex, flexDirection column, justifyContent spaceBetween, alignItems center ] ]
                        [ typography Instructions
                            p
                            [ css [ textAlign center ] ]
                            "Vous devez trouver des \"sets\" parmi les cartes exposées. Un set est un ensemble de trois cartes dont chacune des quatre caractéristiques est soit totalement identique, soit totalement différente aux deux autres cartes. Les caractéristiques sont : la forme, la couleur, le nombre et l’état de remplissage."
                        , typography Instructions
                            p
                            [ css [ textAlign center, Spacing.marginTop Spacing.S ] ]
                            "Dans l'exemple ci-dessous, toutes les lignes, colonnes et diagonales forment des \"sets\" valides."
                        , typography Instructions
                            p
                            [ css [ textAlign center, Spacing.marginTop Spacing.S ] ]
                            "Dans la ligne du milieu, on a des formes différentes, des couleurs différentes, toujours deux fois le symbole et toujours un remplissage en hachure."
                        , typography Instructions
                            p
                            [ css [ textAlign center, Spacing.marginTop Spacing.S ] ]
                            "Dans la dernière colonne, on a des formes différentes, une couleur identique, des nombres de symbole différents et des remplissages différents."
                        , div
                            [ css [ displayFlex, flexDirection column, Spacing.padding Spacing.M ]
                            ]
                            [ div [ css [ displayFlex, flexDirection row ] ]
                                [ viewCard NoPicking { shape = Rectangle, color = Purple, count = Three, fill = FillPlain }
                                    -- Yeah Charly, I know, this is creepy, but you know what? I don't care!!!!
                                    |> Html.Styled.map GameMsg
                                , viewCard NoPicking { shape = Wave, color = Red, count = Three, fill = FillPlain }
                                    |> Html.Styled.map GameMsg
                                , viewCard NoPicking { shape = Diamond, color = Green, count = Three, fill = FillPlain }
                                    |> Html.Styled.map GameMsg
                                ]
                            , div [ css [ displayFlex, flexDirection row ] ]
                                [ viewCard NoPicking { shape = Wave, color = Purple, count = Two, fill = FillStripped }
                                    |> Html.Styled.map GameMsg
                                , viewCard NoPicking { shape = Diamond, color = Red, count = Two, fill = FillStripped }
                                    |> Html.Styled.map GameMsg
                                , viewCard NoPicking { shape = Rectangle, color = Green, count = Two, fill = FillStripped }
                                    |> Html.Styled.map GameMsg
                                ]
                            , div [ css [ displayFlex, flexDirection row ] ]
                                [ viewCard NoPicking { shape = Diamond, color = Purple, count = One, fill = FillEmpty }
                                    |> Html.Styled.map GameMsg
                                , viewCard NoPicking { shape = Rectangle, color = Red, count = One, fill = FillEmpty }
                                    |> Html.Styled.map GameMsg
                                , viewCard NoPicking { shape = Wave, color = Green, count = One, fill = FillEmpty }
                                    |> Html.Styled.map GameMsg
                                ]
                            ]
                        , typography Instructions
                            p
                            [ css [ textAlign center, Spacing.marginTop Spacing.S ] ]
                            "Si vous ne trouvez pas de \"set\" valide, cliquez sur \"Il n'y a pas de set\", trois cartes supplémentaires seront ajoutées !"
                        , button
                            [ onClick ClickedStart, type_ "button", css [ cursor pointer ] ]
                            [ text "Commencer !" ]
                        ]

                InProgress game ->
                    let
                        board =
                            div [ css [ Css.displayFlex, flexDirection column, justifyContent spaceBetween, alignItems center ] ]
                                [ button
                                    [ onClick (GameMsg ClickedNoTripleFound), type_ "button", css [ cursor pointer ] ]
                                    [ text "Il n'y a pas de \"set\" !" ]
                                , viewBoard game.picking game.board
                                    |> Html.Styled.map GameMsg
                                ]
                    in
                    case game.feedback of
                        NoFeedback ->
                            board

                        ValidTriple _ ->
                            popin
                                { background = board
                                , message =
                                    div
                                        [ css
                                            [ DesignSystem.Typography.fontSize DesignSystem.Typography.XL
                                            , Css.fontWeight (Css.int 1000)
                                            , Css.color (Css.hex "004620")
                                            , Css.zIndex (Css.int 20)
                                            , Spacing.padding Spacing.M
                                            , Css.borderRadius (Css.px 5)
                                            , Css.backgroundColor (Css.hex "a3c2b1")
                                            , Css.property "box-shadow" "0 10px 8px 0 rgba(0, 0, 0, .18), 0 5px 5px 0 rgba(0, 0, 0, .15)"
                                            ]
                                        ]
                                        [ text "BRAVO !" ]
                                }

                        InvalidTriple ->
                            popin
                                { background = board
                                , message =
                                    div
                                        [ css
                                            [ DesignSystem.Typography.fontSize DesignSystem.Typography.XL
                                            , Css.fontWeight (Css.int 1000)
                                            , Css.color (Css.hex "f63333")
                                            , Css.zIndex (Css.int 20)
                                            , Spacing.padding Spacing.M
                                            , Css.borderRadius (Css.px 5)
                                            , Css.backgroundColor (Css.hex "dca8a8")
                                            , Css.property "box-shadow" "0 10px 8px 0 rgba(0, 0, 0, .18), 0 5px 5px 0 rgba(0, 0, 0, .15)"
                                            ]
                                        ]
                                        [ text "SET INVALIDE !" ]
                                }

                        ValidTripleExists ->
                            popin
                                { background = board
                                , message =
                                    div
                                        [ css
                                            [ DesignSystem.Typography.fontSize DesignSystem.Typography.XL
                                            , Css.fontWeight (Css.int 1000)
                                            , Css.color (Css.hex "f63333")
                                            , Css.zIndex (Css.int 20)
                                            , Spacing.padding Spacing.M
                                            , Css.borderRadius (Css.px 5)
                                            , Css.backgroundColor (Css.hex "dca8a8")
                                            , Css.property "box-shadow" "0 10px 8px 0 rgba(0, 0, 0, .18), 0 5px 5px 0 rgba(0, 0, 0, .15)"
                                            ]
                                        ]
                                        [ text "UN SET VALIDE EXISTE !" ]
                                }

                Done data ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] "Félicitations !"
                        , typography HeroText
                            p
                            [ css [ marginTop Spacing.M ] ]
                            ("Vous avez trouvé "
                                ++ String.fromInt data.validTriples
                                ++ " sets valides"
                                ++ (case data.mistakes of
                                        0 ->
                                            " sans jamais vous tromper !"

                                        1 ->
                                            " en vous trompant une seule fois !"

                                        n ->
                                            " en vous trompent seulement " ++ String.fromInt n ++ " fois!"
                                   )
                            )
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 11 ]
                        , p [] [ twitterLink 11 ]
                        ]
            , homeLink
            ]


popin : { background : Html msg, message : Html msg } -> Html msg
popin { background, message } =
    div [ css [ Css.position Css.relative ] ]
        [ background
        , div
            [ css
                [ Css.position Css.absolute
                , Css.top (Css.px 0)
                , Css.left (Css.px 0)
                , Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.displayFlex
                , flexDirection column
                , justifyContent center
                , alignItems center
                , zIndex (Css.int 10)
                ]
            ]
            [ message
            ]
        ]


type alias Game =
    { board : List Card
    , deck : List Card
    , validTriples : Int
    , mistakes : Int
    , picking : Picking
    , feedback : Feedback
    }


type GameMsg
    = Picked Card
    | ClickedNoTripleFound
    | FeedbackTimedOut


type Picking
    = NoPicking
    | PickingOne Card
    | PickingTwo ( Card, Card )
    | PickingThree ( Card, Card, Card )


gameInit : Random.Seed -> Game
gameInit seed =
    let
        ( board, deck ) =
            Random.step (Random.List.shuffle cards) seed
                |> Tuple.first
                |> List.Extra.splitAt 12
    in
    { board = board
    , deck = deck
    , validTriples = 0
    , mistakes = 0
    , picking = NoPicking
    , feedback = NoFeedback
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ReadInstruction ->
            Sub.none

        InProgress { feedback } ->
            case feedback of
                NoFeedback ->
                    Sub.none

                _ ->
                    Time.every 2000 (always (GameMsg FeedbackTimedOut))

        Done _ ->
            Sub.none


updateGame : GameMsg -> Game -> Model
updateGame msg game =
    case msg of
        Picked card ->
            case game.picking of
                NoPicking ->
                    InProgress { game | picking = PickingOne card }

                PickingOne picked ->
                    if card == picked then
                        InProgress game

                    else
                        InProgress { game | picking = PickingTwo (push2 card picked) }

                PickingTwo ( picked1, picked2 ) ->
                    if card /= picked1 && card /= picked2 then
                        let
                            newPicked =
                                ( card, picked1, picked2 )

                            newGame =
                                { game | picking = PickingThree newPicked }
                        in
                        if isSet newPicked then
                            InProgress
                                { newGame
                                    | feedback = ValidTriple newPicked
                                    , validTriples = newGame.validTriples + 1
                                }

                        else
                            InProgress { newGame | feedback = InvalidTriple, mistakes = newGame.mistakes + 1 }

                    else
                        InProgress game

                PickingThree _ ->
                    InProgress game

        ClickedNoTripleFound ->
            if containsSet game.board then
                InProgress { game | feedback = ValidTripleExists, mistakes = game.mistakes + 1 }

            else
                let
                    ( firsts, others ) =
                        List.Extra.splitAt 3 game.deck
                in
                InProgress { game | board = game.board ++ firsts, deck = others }

        FeedbackTimedOut ->
            let
                newGame =
                    { game | feedback = NoFeedback, picking = NoPicking }
            in
            case game.feedback of
                ValidTriple ( picked1, picked2, picked3 ) ->
                    if game.validTriples + game.mistakes < 10 then
                        let
                            ( newCards, newDeck ) =
                                if List.length game.board >= 15 then
                                    ( [], game.deck )

                                else
                                    List.Extra.splitAt 3 game.deck
                        in
                        InProgress
                            { newGame
                                | feedback = NoFeedback
                                , board =
                                    game.board
                                        |> List.filter (\c -> c /= picked1 && c /= picked2 && c /= picked3)
                                        |> (++) newCards
                                , deck = newDeck
                            }

                    else
                        Done { mistakes = game.mistakes, validTriples = game.validTriples }

                _ ->
                    InProgress { newGame | feedback = NoFeedback }


type Feedback
    = NoFeedback
    | ValidTriple (T3 Card)
    | InvalidTriple
    | ValidTripleExists


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
        ReadInstruction ->
            Encode.object [ ( "kind", Encode.string "readInstruction" ) ]

        InProgress game ->
            Encode.object
                [ ( "kind", Encode.string "inProgress" )
                , ( "validTriples", Encode.int game.validTriples )
                , ( "mistakes", Encode.int game.mistakes )
                , ( "board", Encode.list cardEncoder game.board )
                , ( "deck", Encode.list cardEncoder game.deck )
                ]

        Done data ->
            Encode.object
                [ ( "kind", Encode.string "done" )
                , ( "validTriples", Encode.int data.validTriples )
                , ( "mistakes", Encode.int data.mistakes )
                ]


cardEncoder : Card -> Encode.Value
cardEncoder card =
    Encode.object
        [ ( "shape"
          , Encode.string
                (case card.shape of
                    Rectangle ->
                        "rectangle"

                    Wave ->
                        "wave"

                    Diamond ->
                        "diamond"
                )
          )
        , ( "count"
          , Encode.int
                (case card.count of
                    One ->
                        1

                    Two ->
                        2

                    Three ->
                        3
                )
          )
        , ( "color"
          , Encode.string
                (case card.color of
                    Red ->
                        "red"

                    Green ->
                        "green"

                    Purple ->
                        "purple"
                )
          )
        , ( "fill"
          , Encode.string
                (case card.fill of
                    FillEmpty ->
                        "empty"

                    FillPlain ->
                        "plain"

                    FillStripped ->
                        "stripped"
                )
          )
        ]


stateDecoder : Decode.Decoder Model
stateDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\k ->
                case k of
                    "readInstruction" ->
                        Decode.succeed ReadInstruction

                    "inProgress" ->
                        Decode.map6 Game
                            (Decode.field "board" (Decode.list cardDecoder))
                            (Decode.field "deck" (Decode.list cardDecoder))
                            (Decode.field "validTriples" Decode.int)
                            (Decode.field "mistakes" Decode.int)
                            (Decode.succeed NoPicking)
                            (Decode.succeed NoFeedback)
                            |> Decode.map InProgress

                    "done" ->
                        Decode.map2 DoneData
                            (Decode.field "validTriples" Decode.int)
                            (Decode.field "mistakes" Decode.int)
                            |> Decode.map Done

                    _ ->
                        Decode.fail ("unknown kind: " ++ k)
            )


cardDecoder : Decode.Decoder Card
cardDecoder =
    Decode.map4 Card
        (Decode.field "shape" Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "rectangle" ->
                            Decode.succeed Rectangle

                        "wave" ->
                            Decode.succeed Wave

                        "diamond" ->
                            Decode.succeed Diamond

                        _ ->
                            Decode.fail ("unknown shape: " ++ s)
                )
        )
        (Decode.field "count" Decode.int
            |> Decode.andThen
                (\s ->
                    case s of
                        1 ->
                            Decode.succeed One

                        2 ->
                            Decode.succeed Two

                        3 ->
                            Decode.succeed Three

                        _ ->
                            Decode.fail ("invalid count: " ++ String.fromInt s)
                )
        )
        (Decode.field "fill" Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "empty" ->
                            Decode.succeed FillEmpty

                        "plain" ->
                            Decode.succeed FillPlain

                        "stripped" ->
                            Decode.succeed FillStripped

                        _ ->
                            Decode.fail ("unknown fill: " ++ s)
                )
        )
        (Decode.field "color" Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "red" ->
                            Decode.succeed Red

                        "green" ->
                            Decode.succeed Green

                        "purple" ->
                            Decode.succeed Purple

                        _ ->
                            Decode.fail ("unknown color: " ++ s)
                )
        )
