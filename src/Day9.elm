module Day9 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (absolute, alignItems, backgroundColor, block, border3, borderBottomWidth, borderColor, borderLeftColor, borderRightColor, calc, center, cursor, display, displayFlex, flexWrap, height, inlineBlock, justifyContent, left, minus, notAllowed, paddingLeft, pct, pointer, position, px, relative, right, solid, spaceAround, textAlign, top, transform, translateX, transparent, width, wrap, zero)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Colors as Colors
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (SpacingSize(..), margin2, marginBottom, marginLeft, marginRight, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, h1, li, p, text, ul)
import Html.Styled.Attributes exposing (class, css, type_)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty)
import Time exposing (Posix, Zone)


type alias Event =
    { label : String
    , date : String
    }


type Order
    = First
    | Second
    | Third
    | Fourth
    | Fifth


type alias Enigma =
    { events : List ( Event, Order )
    }


firstEnigma : Enigma
firstEnigma =
    { events =
        [ ( { label = "Invention du croissant", date = "900" }, Second )
        , ( { label = "Premier Homme dans l'espace", date = "1961" }, Fifth )
        , ( { label = "Invention de la brosse à dents", date = "1498" }, Third )
        , ( { label = "Création de l'ours en peluche", date = "1902" }, Fourth )
        , ( { label = "Première bouteille en verre", date = "-200" }, First )
        ]
    }


secondEnigma : Enigma
secondEnigma =
    { events =
        [ ( { label = "Inauguration du Colisée", date = "80" }, Third )
        , ( { label = "Invention de la montgolfière", date = "1783" }, Fourth )
        , ( { label = "Invention de la catapulte", date = "-397" }, First )
        , ( { label = "La révolte de Spartacus", date = "-73" }, Second )
        , ( { label = "Invention du taille-crayon", date = "1828" }, Fifth )
        ]
    }


type EnigmaState
    = FirstEvent { wronglySelectedEvents : List ( Event, Order ) }
    | RemainingEvents { previousScores : Nonempty Int, previousEvents : Nonempty Event, remainingEvents : Nonempty ( Event, Order ), wronglySelectedEvents : List ( Event, Order ), step : Order }
    | Done { scores : List Int, events : List Event }


type Model
    = FirstEnigma EnigmaState
    | SecondEnigma Int EnigmaState
    | DayDone Int


init : Model
init =
    FirstEnigma (FirstEvent { wronglySelectedEvents = [] })


type Msg
    = Try ( Event, Order )
    | Continue


update : Model -> Msg -> Model
update modelState msg =
    case modelState of
        FirstEnigma state ->
            case updateState firstEnigma state msg of
                SameEnigma newState ->
                    FirstEnigma newState

                NextEnigma score ->
                    SecondEnigma score (FirstEvent { wronglySelectedEvents = [] })

        SecondEnigma firstEnigmaScore state ->
            case updateState secondEnigma state msg of
                SameEnigma newState ->
                    SecondEnigma firstEnigmaScore newState

                NextEnigma score ->
                    DayDone (firstEnigmaScore + score)

        DayDone score ->
            DayDone score


type UpdateResult
    = SameEnigma EnigmaState
    | NextEnigma Int


updateState : Enigma -> EnigmaState -> Msg -> UpdateResult
updateState enigma state msg =
    case ( state, msg ) of
        ( FirstEvent { wronglySelectedEvents }, Try ( event, order ) ) ->
            if order == First then
                let
                    remainingEventsMaybe =
                        List.filter (\( _, eventOrder ) -> order /= eventOrder) enigma.events
                            |> Nonempty.fromList

                    score =
                        List.length wronglySelectedEvents |> getScoreFromFailedNumber
                in
                case remainingEventsMaybe of
                    Just remainingEvents ->
                        SameEnigma
                            (RemainingEvents
                                { previousScores = Nonempty.fromElement score
                                , previousEvents = Nonempty.fromElement event
                                , remainingEvents = remainingEvents
                                , wronglySelectedEvents = []
                                , step = Second
                                }
                            )

                    Nothing ->
                        SameEnigma (Done { scores = [ score ], events = [ event ] })

            else
                SameEnigma (FirstEvent { wronglySelectedEvents = ( event, order ) :: wronglySelectedEvents })

        ( RemainingEvents { previousScores, previousEvents, remainingEvents, wronglySelectedEvents, step }, Try ( event, order ) ) ->
            if order == step then
                let
                    remainingEventsMaybe =
                        List.filter (\( _, eventOrder ) -> order /= eventOrder) (Nonempty.toList remainingEvents)
                            |> Nonempty.fromList

                    score =
                        List.length wronglySelectedEvents |> getScoreFromFailedNumber
                in
                case remainingEventsMaybe of
                    Just newRemainingEvents ->
                        SameEnigma
                            (RemainingEvents
                                { previousScores = Nonempty.cons score previousScores
                                , previousEvents = Nonempty.cons event previousEvents
                                , remainingEvents = newRemainingEvents
                                , wronglySelectedEvents = []
                                , step = nextOrder step
                                }
                            )

                    Nothing ->
                        SameEnigma
                            (Done
                                { scores = Nonempty.cons score previousScores |> Nonempty.toList
                                , events = Nonempty.cons event previousEvents |> Nonempty.toList |> List.reverse
                                }
                            )

            else
                SameEnigma
                    (RemainingEvents
                        { previousScores = previousScores
                        , previousEvents = previousEvents
                        , remainingEvents = remainingEvents
                        , wronglySelectedEvents = ( event, order ) :: wronglySelectedEvents
                        , step = step
                        }
                    )

        ( Done _, Try _ ) ->
            SameEnigma state

        ( Done _, Continue ) ->
            NextEnigma (getEnigmaStateScore state)

        ( _, Continue ) ->
            SameEnigma state


getScoreFromFailedNumber : Int -> Int
getScoreFromFailedNumber failedAttempts =
    if failedAttempts > 3 then
        0

    else if failedAttempts >= 2 then
        1

    else if failedAttempts == 1 then
        2

    else
        3


nextOrder : Order -> Order
nextOrder order =
    case order of
        First ->
            Second

        Second ->
            Third

        Third ->
            Fourth

        Fourth ->
            Fifth

        Fifth ->
            Fifth


getScore : Model -> Int
getScore state =
    case state of
        DayDone score ->
            score

        FirstEnigma enigmaState ->
            getEnigmaStateScore enigmaState

        SecondEnigma firstEnigmaScore enigmaState ->
            firstEnigmaScore + getEnigmaStateScore enigmaState


getEnigmaStateScore : EnigmaState -> Int
getEnigmaStateScore enigmaState =
    case enigmaState of
        FirstEvent _ ->
            0

        RemainingEvents { previousScores } ->
            Nonempty.toList previousScores
                |> List.sum

        Done { scores } ->
            List.sum scores


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay < 9 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day4" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 9 | Timeline"
            , case state of
                FirstEnigma enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Saurez-vous classer ces évènements du plus ancien au plus récent ?"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , viewEnigma firstEnigma enigmaState
                        ]

                SecondEnigma _ enigmaState ->
                    div []
                        [ typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , viewEnigma secondEnigma enigmaState
                        ]

                DayDone _ ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] "Le défi du jour est terminé !"
                        , typography HeroText p [ css [ marginTop Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 9 ]
                        , p [] [ twitterLink 9 ]
                        ]
            , homeLink
            ]


viewEnigma : Enigma -> EnigmaState -> Html Msg
viewEnigma enigma enigmaState =
    case enigmaState of
        FirstEvent { wronglySelectedEvents } ->
            div []
                [ typography Instructions p [ css [ textAlign center, marginBottom Spacing.S ] ] "Cliquez sur l'évènement le plus ANCIEN dans la liste suivante :"
                , div [ class "events" ]
                    (List.map
                        (\event ->
                            if List.member event wronglySelectedEvents then
                                viewWronglySelectedEvent event

                            else
                                viewEvent event
                        )
                        enigma.events
                    )
                ]

        RemainingEvents data ->
            div []
                [ div [ css [ textAlign center, marginBottom Spacing.L ] ]
                    [ ul [ class "foundEvents" ] (List.map viewFoundEvent (Nonempty.toList data.previousEvents) |> List.reverse)
                    ]
                , typography Instructions p [ css [ textAlign center, marginBottom Spacing.S ] ] "Bravo ! Trouvez l'évènement suivant le plus ANCIEN :"
                , div [ class "events" ]
                    (List.map
                        (\event ->
                            if List.member event data.wronglySelectedEvents then
                                viewWronglySelectedEvent event

                            else
                                viewEvent event
                        )
                        (Nonempty.toList data.remainingEvents)
                    )
                ]

        Done data ->
            div []
                [ div [ css [ textAlign center, marginBottom Spacing.L ] ]
                    [ ul [ class "foundEvents" ] (List.map viewFoundEvent data.events)
                    ]
                , typography Instructions p [ css [ textAlign center, marginBottom Spacing.S ] ] "Bravo ! Vous avez mis de l'ordre dans ces évènements !"
                , div [ css [ textAlign center ] ]
                    [ button [ type_ "button", onClick Continue ] [ text "Continuer" ]
                    ]
                ]


viewFoundEvent : Event -> Html msg
viewFoundEvent event =
    li [ class "foundEvent" ] [ text (event.label ++ " (" ++ event.date ++ ")") ]


viewEvent : ( Event, Order ) -> Html Msg
viewEvent ( event, order ) =
    button [ type_ "button", class "event", onClick (Try ( event, order )) ] [ text event.label ]


viewWronglySelectedEvent : ( Event, Order ) -> Html msg
viewWronglySelectedEvent ( event, order ) =
    button [ type_ "button", class "event event--wrong" ] [ text event.label ]


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

        DayDone score ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "score", Encode.int score )
                ]


encodeEnigmaState : EnigmaState -> Encode.Value
encodeEnigmaState enigmaState =
    case enigmaState of
        FirstEvent { wronglySelectedEvents } ->
            Encode.object
                [ ( "step", Encode.string "first-event" )
                , ( "wrongly-selected-events", encodeEventList wronglySelectedEvents )
                ]

        RemainingEvents data ->
            Encode.object
                [ ( "step", Encode.string "remaining-events" )
                , ( "previous-scores", Encode.list Encode.int (Nonempty.toList data.previousScores) )
                , ( "previous-events", Encode.list (.label >> Encode.string) (Nonempty.toList data.previousEvents) )
                , ( "remaining-events", Encode.list (Tuple.first >> .label >> Encode.string) (Nonempty.toList data.remainingEvents) )
                , ( "wrongly-selected-events", encodeEventList data.wronglySelectedEvents )
                , ( "number", encodeOrder data.step )
                ]

        Done data ->
            Encode.object
                [ ( "step", Encode.string "done" )
                , ( "scores", Encode.list Encode.int data.scores )
                , ( "events", Encode.list (.label >> Encode.string) data.events )
                ]


encodeEventList : List ( Event, Order ) -> Encode.Value
encodeEventList events =
    Encode.list Encode.string (List.map (Tuple.first >> .label) events)


encodeOrder : Order -> Encode.Value
encodeOrder order =
    case order of
        First ->
            Encode.int 1

        Second ->
            Encode.int 2

        Third ->
            Encode.int 3

        Fourth ->
            Encode.int 4

        Fifth ->
            Encode.int 5


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
                    "first-event" ->
                        Decode.map (\wronglySelectedEvents -> FirstEvent { wronglySelectedEvents = wronglySelectedEvents })
                            (Decode.field "wrongly-selected-events" (Decode.list (eventOrderDecoder enigma)))

                    "remaining-events" ->
                        Decode.map5
                            (\previousScores previousEvents remainingEvents wronglySelectedEvents stepOrder ->
                                RemainingEvents
                                    { previousScores = previousScores
                                    , previousEvents = previousEvents
                                    , remainingEvents = remainingEvents
                                    , wronglySelectedEvents = wronglySelectedEvents
                                    , step = stepOrder
                                    }
                            )
                            (Decode.field "previous-scores" (nonEmptyListDecoder Decode.int))
                            (Decode.field "previous-events" (nonEmptyListDecoder (eventOrderDecoder enigma) |> Decode.map (Nonempty.map Tuple.first)))
                            (Decode.field "remaining-events" (nonEmptyListDecoder (eventOrderDecoder enigma)))
                            (Decode.field "wrongly-selected-events" (Decode.list (eventOrderDecoder enigma)))
                            (Decode.field "number" orderDecoder)

                    "done" ->
                        Decode.map2 (\scores events -> Done { scores = scores, events = events })
                            (Decode.field "scores" (Decode.list Decode.int))
                            (Decode.field "second-wrong-answer" (Decode.list (eventOrderDecoder enigma) |> Decode.map (List.map Tuple.first)))

                    _ ->
                        Decode.fail ("Invalid step: " ++ step)
            )


eventOrderDecoder : Enigma -> Decoder ( Event, Order )
eventOrderDecoder enigma =
    Decode.string
        |> Decode.andThen
            (\eventLabel ->
                List.find (\( { label }, _ ) -> label == eventLabel) enigma.events
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "Event not found")
            )


orderDecoder : Decoder Order
orderDecoder =
    Decode.int
        |> Decode.andThen
            (\number ->
                case number of
                    1 ->
                        Decode.succeed First

                    2 ->
                        Decode.succeed Second

                    3 ->
                        Decode.succeed Third

                    4 ->
                        Decode.succeed Fourth

                    5 ->
                        Decode.succeed Fifth

                    _ ->
                        Decode.fail "Invalid number"
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
    [ Css.class "events"
        [ displayFlex
        , alignItems center
        , justifyContent spaceAround
        , flexWrap wrap
        , Css.children
            [ Css.class "event"
                [ marginTop Spacing.M
                , marginLeft Spacing.S
                , marginRight Spacing.S
                , cursor pointer
                , Css.withClass "event--wrong"
                    [ borderColor Colors.secondary
                    , backgroundColor Colors.secondary
                    , cursor notAllowed
                    ]
                ]
            ]
        ]
    , Css.class "foundEvents"
        [ display inlineBlock
        , paddingLeft (px 50)
        , position relative
        , textAlign left
        , Css.children
            [ Css.class "foundEvent"
                [ margin2 Spacing.M Spacing.NoSpace
                ]
            ]
        , Css.before
            [ Css.property "content" "''"
            , width (px 10)
            , height (calc (pct 100) minus (px 10))
            , position absolute
            , left (px 20)
            , top zero
            , backgroundColor Colors.secondary
            ]
        , Css.after
            [ Css.property "content" "''"
            , width zero
            , height zero
            , display block
            , border3 (px 10) solid Colors.secondary
            , borderLeftColor transparent
            , borderRightColor transparent
            , borderBottomWidth zero
            , position absolute
            , left (px 20)
            , top (calc (pct 100) minus (px 10))
            , transform (translateX (pct -25))
            ]
        ]
    ]
