module Day1 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (absolute, backgroundColor, block, borderRadius, calc, capitalize, center, display, displayFlex, em, flexWrap, fontSize, height, justifyContent, left, margin, minWidth, minus, paddingTop, pct, position, px, relative, rgb, right, textAlign, textTransform, top, uppercase, width, wrap)
import Css.Global as Css exposing (Snippet)
import Css.Media as Media
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop, padding2)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, h1, p, ul)
import Html.Styled.Attributes exposing (class, css, href, type_)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Set exposing (Set)
import Time exposing (Posix, Zone)
import Utils.Html exposing (attributeIf)


type alias Enigma =
    { words : Set String, answers : Set String, clue : String }


type alias CurrentEnigma =
    { words : Set String
    , correctAnswers : Set String
    , remainingAnswers : Set String
    , wrongAnswers : Set String
    , clue : String
    }


type Model
    = InProgress { done : List Int, current : CurrentEnigma, remaining : List Enigma }
    | Done Int


init : Model
init =
    InProgress { done = [], current = firstEnigma, remaining = otherEnigmas }


firstEnigma : CurrentEnigma
firstEnigma =
    { words = Set.fromList [ "chou", "noir", "pingouin", "flèche", "vanne", "boue", "bar", "somme", "ours", "brise", "médecine", "démarche", "las vegas", "pâques", "tarte", "tennis", "ambulance", "nez", "Barcelone", "cavalier", "signe", "panda", "double", "droit", "élastique" ]
    , correctAnswers = Set.empty
    , remainingAnswers = Set.fromList [ "pingouin", "ours", "panda", "bar" ]
    , wrongAnswers = Set.empty
    , clue = "animaux"
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { words = Set.fromList [ "restaurant", "loup", "service", "bébé", "lunette", "fenêtre", "disque", "élastique", "lacet", "cochon", "moustache", "classe", "jumelles", "agent", "banane", "couronne", "modèle", "python", "quart", "bain", "face", "bronze", "télescope", "chanteur", "trésor" ]
      , answers = Set.fromList [ "élastique", "lacet" ]
      , clue = "attacher"
      }
    , { words = Set.fromList [ "cimetière", "air", "franchise", "phare", "offre", "ballon", "président", "échelle", "soupe", "porcelaine", "ceinture", "louche", "café", "anneau", "New-York", "moteur", "ombre", "plomb", "code", "épice", "oreiller", "note", "clown", "ange", "Berlin" ]
      , answers = Set.fromList [ "ombre", "oreiller", "phare" ]
      , clue = "nuit"
      }
    ]


type Msg
    = Try String


update : Model -> Msg -> Model
update state msg =
    case ( state, msg ) of
        ( InProgress model, Try word ) ->
            let
                newCurrent =
                    if Set.member word model.current.remainingAnswers then
                        let
                            current =
                                model.current
                        in
                        { current | remainingAnswers = Set.filter ((/=) word) model.current.remainingAnswers, correctAnswers = Set.insert word model.current.correctAnswers }

                    else if Set.member word model.current.correctAnswers then
                        model.current

                    else
                        let
                            current =
                                model.current
                        in
                        { current | wrongAnswers = Set.insert word model.current.wrongAnswers }
            in
            if Set.isEmpty newCurrent.remainingAnswers then
                case model.remaining of
                    firstRemaining :: others ->
                        InProgress
                            { model
                                | current =
                                    { words = firstRemaining.words
                                    , correctAnswers = Set.empty
                                    , remainingAnswers = firstRemaining.answers
                                    , wrongAnswers = Set.empty
                                    , clue = firstRemaining.clue
                                    }
                                , remaining = others
                                , done = getCurrentEnigmaScore newCurrent :: model.done
                            }

                    [] ->
                        Done (getScore newCurrent model.done)

            else
                InProgress { model | current = newCurrent }

        ( Done _, _ ) ->
            state


getScore : CurrentEnigma -> List Int -> Int
getScore currentEnigma done =
    getCurrentEnigmaScore currentEnigma + List.sum done


getCurrentEnigmaScore : CurrentEnigma -> Int
getCurrentEnigmaScore currentEnigma =
    Set.size currentEnigma.correctAnswers * 2 - Set.size currentEnigma.wrongAnswers


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay <= 1 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day1" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Premier jour | Codenames"
            , case state of
                InProgress model ->
                    let
                        answersCount =
                            Set.size model.current.correctAnswers + Set.size model.current.remainingAnswers
                    in
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Veuillez retrouver les mots correspondant à l'indice et au nombre de mots suivant ci-dessous :"
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.M, marginTop Spacing.S, textTransform uppercase ] ] (model.current.clue ++ " – " ++ String.fromInt answersCount ++ " mots")
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore model.current model.done))
                        , ul [ class "grid" ] (List.map (viewWord model.current) (Set.toList model.current.words))
                        ]

                Done score ->
                    typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] ("Défi terminé ! Votre score : " ++ String.fromInt score)
            ]


isDone : Model -> Bool
isDone model =
    case model of
        Done _ ->
            True

        InProgress _ ->
            False


saveState : Model -> Encode.Value
saveState state =
    case state of
        InProgress model ->
            Encode.object
                [ ( "state", Encode.string "in-progress" )
                , ( "done", Encode.list Encode.int model.done )
                , ( "correctAnswers", Encode.set Encode.string model.current.correctAnswers )
                , ( "remainingAnswers", Encode.set Encode.string model.current.remainingAnswers )
                , ( "wrongAnswers", Encode.set Encode.string model.current.wrongAnswers )
                ]

        Done score ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "score", Encode.int score )
                ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "in-progress" ->
                        Decode.map4
                            (\done correctAnswers remainingAnswers wrongAnswers ->
                                if List.isEmpty done then
                                    InProgress
                                        { done = done
                                        , remaining = otherEnigmas
                                        , current =
                                            { firstEnigma
                                                | correctAnswers = correctAnswers
                                                , remainingAnswers = remainingAnswers
                                                , wrongAnswers = wrongAnswers
                                            }
                                        }
                                        |> Decode.succeed

                                else
                                    let
                                        remainingEnigmas =
                                            List.drop (List.length done - 1) otherEnigmas
                                    in
                                    case remainingEnigmas of
                                        firstRemaining :: others ->
                                            InProgress
                                                { done = done
                                                , remaining = others
                                                , current =
                                                    { words = firstRemaining.words
                                                    , correctAnswers = correctAnswers
                                                    , remainingAnswers = remainingAnswers
                                                    , wrongAnswers = wrongAnswers
                                                    , clue = firstRemaining.clue
                                                    }
                                                }
                                                |> Decode.succeed

                                        [] ->
                                            Decode.fail "Invalid state"
                            )
                            (Decode.field "done" (Decode.list Decode.int))
                            (Decode.field "correctAnswers" (Decode.set Decode.string))
                            (Decode.field "remainingAnswers" (Decode.set Decode.string))
                            (Decode.field "wrongAnswers" (Decode.set Decode.string))
                            |> Decode.andThen identity

                    "done" ->
                        Decode.map Done (Decode.field "score" Decode.int)

                    stateValue ->
                        Decode.fail ("Unknown state value: " ++ stateValue)
            )


viewWord : CurrentEnigma -> String -> Html Msg
viewWord currentEnigma word =
    button
        [ class "card"
        , href "#"
        , type_ "button"
        , onClick (Try word)
        , attributeIf (Set.member word currentEnigma.correctAnswers) (class "card--correct")
        , attributeIf (Set.member word currentEnigma.wrongAnswers) (class "card--wrong")
        ]
        [ typography Paragraph div [ class "word" ] word ]


styles : List Snippet
styles =
    [ Css.class "grid"
        [ displayFlex
        , flexWrap wrap
        , justifyContent center
        ]
    , Css.class "card"
        [ borderRadius (px 5)
        , backgroundColor (rgb 236 200 87)
        , width (calc (pct 20) minus (px 10))
        , minWidth (px 120)
        , height (px 80)
        , margin (px 5)
        , paddingTop (px 10)
        , position relative
        , Css.children
            [ Css.class "word"
                [ position absolute
                , display block
                , left (px 10)
                , right (px 10)
                , top (px 40)
                , backgroundColor (rgb 246 221 140)
                , padding2 Spacing.XXS Spacing.XS
                , borderRadius (px 5)
                , textTransform uppercase
                , Media.withMedia [ Media.all [ Media.maxWidth (px 500) ] ]
                    [ fontSize (em 0.9) ]
                ]
            ]
        , Css.withClass "card--correct"
            [ backgroundColor (rgb 91 172 74)
            , Css.children
                [ Css.class "word"
                    [ backgroundColor (rgb 217 255 212)
                    ]
                ]
            ]
        , Css.withClass "card--wrong"
            [ backgroundColor (rgb 212 0 0)
            , Css.children
                [ Css.class "word"
                    [ backgroundColor (rgb 255 169 169)
                    ]
                ]
            ]
        ]
    ]
