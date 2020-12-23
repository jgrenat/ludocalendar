module Day24 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (Px, absolute, backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, bold, borderWidth, bottom, center, color, contain, displayFlex, fontWeight, justifyContent, left, maxHeight, maxWidth, noRepeat, none, paddingTop, pct, position, relative, rgb, right, spaceAround, textAlign, top, touchAction, transparent, url, vh, width, zero)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Colors as Colors
import DesignSystem.Link exposing (homeLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Events.Extra.Pointer as Pointer exposing (Event)
import Html.Styled exposing (Html, button, div, form, h1, img, input, p, text)
import Html.Styled.Attributes exposing (class, classList, css, fromUnstyled, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http exposing (stringBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Nonempty as Nonempty exposing (Nonempty)
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import Ports
import String.Normalize exposing (removeDiacritics)
import Svg.Styled exposing (Svg, line, svg)
import Svg.Styled.Attributes as Svg exposing (stroke, strokeWidth, x1, x2, y1, y2)
import Time exposing (Posix, Zone)
import Utils.Html exposing (viewIf, viewMaybe)


type alias Enigma =
    { symbols : String, answers : Nonempty String }


type Model
    = FirstEnigma EnigmaState
    | SecondEnigma Int EnigmaState
    | ThirdEnigma Int EnigmaState
    | DayDone Int String Bool


type alias EnigmaState =
    { failedAttempts : Int
    , lastFailedAttempt : Maybe String
    , lastPaintedPosition : Maybe Position
    , paints : List ( Position, Position )
    , fieldValue : String
    }


init : Model
init =
    FirstEnigma
        { failedAttempts = 0
        , lastFailedAttempt = Nothing
        , lastPaintedPosition = Nothing
        , paints = []
        , fieldValue = ""
        }


firstEnigma : Enigma
firstEnigma =
    { symbols = ImagePath.toString images.day24.drawing1, answers = Nonempty.Nonempty "maison" [ "une maison", "la maison" ] }


secondEnigma : Enigma
secondEnigma =
    { symbols = ImagePath.toString images.day24.drawing2, answers = Nonempty.Nonempty "lettre" [ "une lettre", "la lettre", "enveloppe", "une enveloppe", "l'enveloppe", "lettres", "enveloppes", "des enveloppes", "des lettres", "courrier", "un courrier", "du courrier" ] }


thirdEnigma : Enigma
thirdEnigma =
    { symbols = ImagePath.toString images.day24.drawing3, answers = Nonempty.Nonempty "pull" [ "un pull", "le pull", "t-shirt", "un t-shirt", "tee-shirt", "un tee-shirt", "le tee-shirt", "le t-shirt", "pull-over", "un pull-over", "le pull-over", "maillot", "un maillot", "le maillot" ] }


type alias Position =
    ( Float, Float )


type Msg
    = Start
    | Try String
    | StartPainting Event
    | StopPainting
    | Paint Event
    | ClearPaints
    | SaveEmail String
    | FieldChanged String
    | EmailSaved
    | EmailChanged String


update : Model -> Msg -> ( Model, Cmd Msg )
update state msg =
    case ( state, msg ) of
        ( FirstEnigma enigmaState, _ ) ->
            case updateState firstEnigma enigmaState msg of
                SameEnigma newState ->
                    ( FirstEnigma newState, Cmd.none )

                NextEnigma score ->
                    ( SecondEnigma score
                        { failedAttempts = 0
                        , lastFailedAttempt = Nothing
                        , lastPaintedPosition = Nothing
                        , paints = []
                        , fieldValue = ""
                        }
                    , Cmd.none
                    )

        ( SecondEnigma firstEnigmaScore enigmaState, _ ) ->
            case updateState secondEnigma enigmaState msg of
                SameEnigma newState ->
                    ( SecondEnigma firstEnigmaScore newState, Cmd.none )

                NextEnigma score ->
                    ( ThirdEnigma (firstEnigmaScore + score)
                        { failedAttempts = 0
                        , lastFailedAttempt = Nothing
                        , lastPaintedPosition = Nothing
                        , paints = []
                        , fieldValue = ""
                        }
                    , Cmd.none
                    )

        ( ThirdEnigma firstEnigmasScore enigmaState, _ ) ->
            case updateState thirdEnigma enigmaState msg of
                SameEnigma newState ->
                    ( ThirdEnigma firstEnigmasScore newState, Cmd.none )

                NextEnigma score ->
                    ( DayDone (firstEnigmasScore + score) "" False, Ports.initComments () )

        ( DayDone score emailField _, SaveEmail email ) ->
            ( DayDone score emailField False, saveEmail email )

        ( DayDone score _ _, EmailChanged newValue ) ->
            ( DayDone score newValue False, Cmd.none )

        ( DayDone score _ _, EmailSaved ) ->
            ( DayDone score "" True, Cmd.none )

        ( DayDone _ _ _, _ ) ->
            ( state, Cmd.none )


saveEmail : String -> Cmd Msg
saveEmail email =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" "Bearer f864369a0205fc10d50e46bc9010a5"
            , Http.header "accept" "application/json"
            ]
        , url = "https://site-api.datocms.com/items"
        , body = stringBody "application/json" ("""{ "data": { "type": "item", "attributes": { "email": \"""" ++ email ++ """" }, "meta": {}, "relationships": { "item_type": { "data": { "type": "item_type", "id": "473383" } } } } }""")
        , expect = Http.expectWhatever (always EmailSaved)
        , timeout = Nothing
        , tracker = Nothing
        }


type UpdateResult
    = SameEnigma EnigmaState
    | NextEnigma Int


normalize : String -> String
normalize string =
    String.replace " " "" string
        |> removeDiacritics
        |> String.toLower


updateState : Enigma -> EnigmaState -> Msg -> UpdateResult
updateState enigma state msg =
    case msg of
        Start ->
            SameEnigma state

        SaveEmail _ ->
            SameEnigma state

        EmailSaved ->
            SameEnigma state

        EmailChanged _ ->
            SameEnigma state

        Try answer ->
            if Nonempty.member (normalize answer) (Nonempty.map normalize enigma.answers) then
                NextEnigma (8 - state.failedAttempts |> max 0)

            else
                SameEnigma { state | failedAttempts = state.failedAttempts + 1, lastFailedAttempt = Just answer }

        StartPainting event ->
            SameEnigma { state | lastPaintedPosition = Just event.pointer.offsetPos }

        StopPainting ->
            SameEnigma { state | lastPaintedPosition = Nothing }

        Paint event ->
            case state.lastPaintedPosition of
                Just lastPosition ->
                    SameEnigma
                        { state
                            | lastPaintedPosition = Just event.pointer.offsetPos
                            , paints = ( lastPosition, event.pointer.offsetPos ) :: state.paints
                        }

                Nothing ->
                    SameEnigma state

        ClearPaints ->
            SameEnigma { state | lastPaintedPosition = Nothing, paints = [] }

        FieldChanged newValue ->
            SameEnigma { state | fieldValue = newValue }


getScore : Model -> Int
getScore state =
    case state of
        FirstEnigma _ ->
            0

        SecondEnigma firstEnigmaScore _ ->
            firstEnigmaScore

        ThirdEnigma firstEnigmasScore _ ->
            firstEnigmasScore

        DayDone score _ _ ->
            score


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate + 2
    in
    if maxDay < 24 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 24 | Imagicien"
            , case state of
                FirstEnigma enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Reliez les symboles dans l'ordre indiqué à droite pour faire apparaître un dessin, puis devinez ce dont il s'agit !"
                        , viewEnigma firstEnigma enigmaState
                        ]

                SecondEnigma _ enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Reliez les symboles dans l'ordre indiqué à droite pour faire apparaître un dessin, puis devinez ce dont il s'agit !"
                        , viewEnigma secondEnigma enigmaState
                        ]

                ThirdEnigma _ enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Reliez les symboles dans l'ordre indiqué à droite pour faire apparaître un dessin, puis devinez ce dont il s'agit !"
                        , viewEnigma thirdEnigma enigmaState
                        ]

                DayDone _ fieldValue isSaved ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography Paragraph p [] "Vous avez terminé le dernier jour de LudoCalendar, félicitations !"
                        , typography Instructions p [ css [ marginTop Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , typography Paragraph p [ css [ marginTop Spacing.M, fontWeight bold, marginBottom Spacing.S ] ] "Si vous voulez être prévenu l'année prochaine pour la nouvelle édition de notre calendrier, vous pouvez laisser votre email (il ne sera utilisé que pour ça)."
                        , form [ onSubmit (SaveEmail fieldValue), css [ textAlign center ] ]
                            [ input [ type_ "text", value fieldValue, onInput EmailChanged ] []
                            , button [ type_ "submit" ] [ text "Valider" ]
                            ]
                        , typography Paragraph p [ css [ color Colors.primary ] ] "Email sauvegardé, à l'année prochaine !"
                            |> viewIf isSaved
                        , typography HeroText p [ css [ marginTop Spacing.M ] ] "🎄 🎁 Nous espérons que vous vous êtes bien amusés avec nous ! Si c'est le cas, n'hésitez pas à nous laisser un commentaire ! 🎁 🎄"
                        , div [ class "commentbox", css [ marginTop Spacing.M ] ] []
                        ]
            , homeLink
            ]


viewEnigma : Enigma -> EnigmaState -> Html Msg
viewEnigma enigma enigmaState =
    div [ css [ marginTop Spacing.M ] ]
        [ viewMaybe
            (\wrongAnswer ->
                typography WrongAnswer
                    p
                    [ css [ textAlign center, color Colors.secondary, marginTop Spacing.M ] ]
                    ("Raté ! \"" ++ wrongAnswer ++ "\" n'est pas la bonne réponse... Essayez encore !")
            )
            enigmaState.lastFailedAttempt
        , div [ class "board" ]
            [ viewDrawingBoard enigmaState
            , viewDrawInstructions enigma
            ]
        , div [ css [ textAlign center, marginTop Spacing.XS, width (pct 80) ] ]
            [ button [ class "reset-button", onClick ClearPaints ] [ text "Effacer vos traits" ]
            ]
        , viewMaybe
            (\wrongAnswer ->
                typography WrongAnswer
                    p
                    [ css [ textAlign center, color Colors.secondary, marginTop Spacing.M ] ]
                    ("Raté ! La réponse n'est pas \"" ++ wrongAnswer ++ "\". Essayez encore !")
            )
            enigmaState.lastFailedAttempt
        , typography HeroText p [ css [ marginTop Spacing.M, textAlign center, marginBottom Spacing.S ] ] "Quel est le mot à trouver ?"
        , form [ onSubmit (Try enigmaState.fieldValue), css [ textAlign center ] ]
            [ input [ type_ "text", value enigmaState.fieldValue, onInput FieldChanged ] []
            , button [ type_ "submit" ] [ text "Valider" ]
            ]
        ]


viewDrawingBoard : EnigmaState -> Html Msg
viewDrawingBoard enigmaState =
    div
        [ class "drawing-board"
        , Pointer.onWithOptions "pointerdown" { preventDefault = True, stopPropagation = True } StartPainting |> fromUnstyled
        , if enigmaState.lastPaintedPosition == Nothing then
            classList []

          else
            Pointer.onWithOptions "pointermove" { preventDefault = True, stopPropagation = True } Paint |> fromUnstyled
        , Pointer.onUp (always StopPainting) |> fromUnstyled
        ]
        [ svg [ Svg.class "paints", Svg.width "100%", Svg.height "100%" ]
            (List.map drawLine enigmaState.paints)
        ]


drawLine : ( Position, Position ) -> Svg msg
drawLine ( start, end ) =
    line
        [ x1 (Tuple.first start |> String.fromFloat)
        , y1 (Tuple.second start |> String.fromFloat)
        , x2 (Tuple.first end |> String.fromFloat)
        , y2 (Tuple.second end |> String.fromFloat)
        , stroke "black"
        , strokeWidth "5"
        ]
        []


viewDrawInstructions : Enigma -> Html msg
viewDrawInstructions enigma =
    img [ class "draw-instructions", src enigma.symbols ] []


isDone : Model -> Bool
isDone model =
    case model of
        DayDone _ _ _ ->
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

        DayDone score _ _ ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "score", Encode.int score )
                ]


encodeEnigmaState : EnigmaState -> Encode.Value
encodeEnigmaState enigmaState =
    Encode.object
        [ ( "failed-attempts", Encode.int enigmaState.failedAttempts )
        ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "first-enigma" ->
                        Decode.map FirstEnigma (Decode.field "enigma-state" enigmaStateDecoder)

                    "second-enigma" ->
                        Decode.map2 SecondEnigma
                            (Decode.field "first-enigma-score" Decode.int)
                            (Decode.field "enigma-state" enigmaStateDecoder)

                    "third-enigma" ->
                        Decode.map2 ThirdEnigma
                            (Decode.field "first-enigmas-score" Decode.int)
                            (Decode.field "enigma-state" enigmaStateDecoder)

                    "done" ->
                        Decode.map (\score -> DayDone score "" False)
                            (Decode.field "score" Decode.int)

                    _ ->
                        Decode.fail "Invalid state"
            )


enigmaStateDecoder : Decoder EnigmaState
enigmaStateDecoder =
    Decode.map
        (\failedAttempts ->
            { failedAttempts = failedAttempts
            , lastFailedAttempt = Nothing
            , lastPaintedPosition = Nothing
            , paints = []
            , fieldValue = ""
            }
        )
        (Decode.field "failed-attempts" Decode.int)


styles : List Snippet
styles =
    let
        backgroundDimensions =
            ImagePath.dimensions images.day24.background
                |> Maybe.withDefault { width = 1280, height = 1280 }
    in
    [ Css.class "board" [ displayFlex, justifyContent spaceAround ]
    , Css.class "drawing-board"
        [ position relative
        , backgroundImage (ImagePath.toString images.day24.background |> url)
        , backgroundSize contain
        , backgroundRepeat noRepeat
        , width (pct 80)
        , touchAction none
        , paddingTop (toFloat backgroundDimensions.height / toFloat backgroundDimensions.width * 80 |> pct)
        , Css.property "cursor" ("url(" ++ ImagePath.toString images.day24.penCursor ++ "), default")
        , Css.descendants
            [ Css.class "paints"
                [ position absolute
                , top zero
                , left zero
                , right zero
                , bottom zero
                ]
            ]
        ]
    , Css.class "draw-instructions"
        [ maxWidth (pct 20)
        , maxHeight (vh 85)
        ]
    , Css.class "reset-button"
        [ backgroundColor transparent
        , borderWidth zero
        , color (rgb 0 0 0)
        , Css.hover [ backgroundColor (rgb 230 230 230) ]
        , Css.focus [ backgroundColor (rgb 210 210 210) ]
        ]
    ]
