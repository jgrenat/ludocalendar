module Day5 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (absolute, backgroundColor, borderStyle, borderWidth, bottom, center, cursor, dashed, displayFlex, flexWrap, int, justifyContent, left, margin, pct, pointer, position, px, relative, rgb, rgba, right, textAlign, textTransform, top, transparent, uppercase, width, wrap, zIndex, zero)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop, padding)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, h1, img, p)
import Html.Styled.Attributes exposing (alt, class, css, src, type_)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import Set exposing (Set)
import Time exposing (Posix, Zone)
import Utils.Html exposing (attributeIf)


type alias Enigma =
    { images : Set String, answers : Set String, clue : String }


type alias CurrentEnigma =
    { images : Set String
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
    { images = [ images.day5.enigma1.day5111, images.day5.enigma1.day5112, images.day5.enigma1.day5113, images.day5.enigma1.day5114, images.day5.enigma1.day5115, images.day5.enigma1.day5121, images.day5.enigma1.day5122, images.day5.enigma1.day5123, images.day5.enigma1.day5124, images.day5.enigma1.day5125, images.day5.enigma1.day5131, images.day5.enigma1.day5132, images.day5.enigma1.day5133, images.day5.enigma1.day5134, images.day5.enigma1.day5135, images.day5.enigma1.day5141, images.day5.enigma1.day5142, images.day5.enigma1.day5143, images.day5.enigma1.day5144, images.day5.enigma1.day5145, images.day5.enigma1.day5151, images.day5.enigma1.day5152, images.day5.enigma1.day5153, images.day5.enigma1.day5154, images.day5.enigma1.day5155 ] |> List.map ImagePath.toString |> Set.fromList
    , correctAnswers = Set.empty
    , remainingAnswers = [ images.day5.enigma1.day5121, images.day5.enigma1.day5115, images.day5.enigma1.day5135, images.day5.enigma1.day5145 ] |> List.map ImagePath.toString |> Set.fromList
    , wrongAnswers = Set.empty
    , clue = "volant"
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { images = [ images.day5.enigma2.day5211, images.day5.enigma2.day5212, images.day5.enigma2.day5213, images.day5.enigma2.day5214, images.day5.enigma2.day5215, images.day5.enigma2.day5221, images.day5.enigma2.day5222, images.day5.enigma2.day5223, images.day5.enigma2.day5224, images.day5.enigma2.day5225, images.day5.enigma2.day5231, images.day5.enigma2.day5232, images.day5.enigma2.day5233, images.day5.enigma2.day5234, images.day5.enigma2.day5235, images.day5.enigma2.day5241, images.day5.enigma2.day5242, images.day5.enigma2.day5243, images.day5.enigma2.day5244, images.day5.enigma2.day5245, images.day5.enigma2.day5251, images.day5.enigma2.day5252, images.day5.enigma2.day5253, images.day5.enigma2.day5254, images.day5.enigma2.day5255 ] |> List.map ImagePath.toString |> Set.fromList
      , answers = [ images.day5.enigma2.day5214, images.day5.enigma2.day5232, images.day5.enigma2.day5252 ] |> List.map ImagePath.toString |> Set.fromList
      , clue = "fil"
      }
    , { images = [ images.day5.enigma3.day5311, images.day5.enigma3.day5312, images.day5.enigma3.day5313, images.day5.enigma3.day5314, images.day5.enigma3.day5315, images.day5.enigma3.day5321, images.day5.enigma3.day5322, images.day5.enigma3.day5323, images.day5.enigma3.day5324, images.day5.enigma3.day5325, images.day5.enigma3.day5331, images.day5.enigma3.day5332, images.day5.enigma3.day5333, images.day5.enigma3.day5334, images.day5.enigma3.day5335, images.day5.enigma3.day5341, images.day5.enigma3.day5342, images.day5.enigma3.day5343, images.day5.enigma3.day5344, images.day5.enigma3.day5345, images.day5.enigma3.day5351, images.day5.enigma3.day5352, images.day5.enigma3.day5353, images.day5.enigma3.day5354, images.day5.enigma3.day5355 ] |> List.map ImagePath.toString |> Set.fromList
      , answers = [ images.day5.enigma3.day5311, images.day5.enigma3.day5322, images.day5.enigma3.day5342 ] |> List.map ImagePath.toString |> Set.fromList
      , clue = "jeu"
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
                                    { images = firstRemaining.images
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
    if maxDay < 5 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day1" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Cinquième jour | Codenames Images"
            , case state of
                InProgress model ->
                    let
                        answersCount =
                            Set.size model.current.correctAnswers + Set.size model.current.remainingAnswers
                    in
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Retrouvez les images correspondant à l'indice ci-dessous."
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore model.current model.done))
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.M, marginTop Spacing.S, textTransform uppercase ] ] (String.fromInt answersCount ++ " images en rapport avec l'indice : " ++ model.current.clue)
                        , Keyed.ul [ class "grid" ] (List.map (\image -> ( image, viewImage model.current image )) (Set.toList model.current.images))
                        ]

                Done score ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] ("Défi terminé ! Votre score : " ++ String.fromInt score)
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 5 ]
                        , p [] [ twitterLink 5 ]
                        ]
            , homeLink
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
                                                    { images = firstRemaining.images
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


viewImage : CurrentEnigma -> String -> Html Msg
viewImage currentEnigma image =
    button
        [ class "card"
        , type_ "button"
        , onClick (Try image)
        , attributeIf (Set.member image currentEnigma.correctAnswers) (class "card--correct")
        , attributeIf (Set.member image currentEnigma.wrongAnswers) (class "card--wrong")
        ]
        [ img [ src image, class "image", alt "image de l'énigme" ] [] ]


styles : List Snippet
styles =
    [ Css.class "grid"
        [ displayFlex
        , flexWrap wrap
        , justifyContent center
        ]
    , Css.class "card"
        [ width (pct 20)
        , margin zero
        , Css.marginTop (px -5)
        , padding Spacing.NoSpace
        , backgroundColor transparent
        , borderWidth zero
        , position relative
        , cursor pointer
        , Css.focus [ backgroundColor (rgb 236 200 87), borderStyle dashed ]
        , Css.children
            [ Css.class "image"
                [ width (pct 100)
                ]
            ]
        , Css.withClass "card--correct"
            [ Css.after
                [ Css.property "content" "''"
                , backgroundColor (rgba 91 172 74 0.4)
                , position absolute
                , top zero
                , bottom zero
                , left zero
                , right zero
                , zIndex (int 1)
                ]
            ]
        , Css.withClass "card--wrong"
            [ Css.after
                [ Css.property "content" "''"
                , backgroundColor (rgba 212 0 0 0.4)
                , position absolute
                , top zero
                , bottom zero
                , left zero
                , right zero
                , zIndex (int 1)
                ]
            ]
        ]
    ]
