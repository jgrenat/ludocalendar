module Day3 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (center, displayFlex, flexWrap, height, justifyContent, maxWidth, pct, px, right, spaceAround, spaceBetween, textAlign, wrap)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, a, button, div, form, h1, img, input, p, span, text)
import Html.Styled.Attributes exposing (alt, class, css, href, src, target, type_, value)
import Html.Styled.Events exposing (onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import Set exposing (Set)
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)


type alias Enigma =
    { images : List String, answers : Set String }


type Model
    = InProgress { tries : Int, current : Enigma, remaining : List Enigma, fieldValue : String }
    | Done Int


init : Model
init =
    InProgress { tries = 0, current = firstEnigma, remaining = otherEnigmas, fieldValue = "" }


firstEnigma : Enigma
firstEnigma =
    { images = [ ImagePath.toString images.day3.day311, ImagePath.toString images.day3.day312, ImagePath.toString images.day3.day313, ImagePath.toString images.day3.day314, ImagePath.toString images.day3.day315, ImagePath.toString images.day3.day316 ]
    , answers = Set.fromList [ "Rudolph", "Rudolph le renne" ]
    }


otherEnigmas : List Enigma
otherEnigmas =
    [ { images = [ ImagePath.toString images.day3.day321, ImagePath.toString images.day3.day322, ImagePath.toString images.day3.day323, ImagePath.toString images.day3.day324 ]
      , answers = Set.fromList [ "Retour vers le futur" ]
      }
    ]


type Msg
    = Try String
    | FieldChanged String


normalize : String -> String
normalize string =
    String.replace " " "" string
        |> removeDiacritics
        |> String.toLower


update : Model -> Msg -> Model
update state msg =
    case ( state, msg ) of
        ( InProgress model, Try answer ) ->
            if Set.member (normalize answer) (Set.map normalize model.current.answers) then
                case model.remaining of
                    firstRemaining :: others ->
                        InProgress
                            { model
                                | tries = model.tries + 1
                                , current = firstRemaining
                                , remaining = others
                                , fieldValue = ""
                            }

                    [] ->
                        Done (model.tries + 1)

            else
                InProgress { model | tries = model.tries + 1, fieldValue = "" }

        ( InProgress model, FieldChanged value ) ->
            InProgress { model | fieldValue = value }

        ( Done _, _ ) ->
            state


getScore : Model -> Int
getScore state =
    case state of
        InProgress model ->
            let
                finished =
                    1 - List.length model.remaining
            in
            finished * 11 - max 0 model.tries

        Done tries ->
            25 - tries


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay < 3 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day2" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 3 | Concept"
            , case state of
                InProgress model ->
                    div []
                        [ p [ css [ textAlign center ] ]
                            [ typography Instructions span [] "Retrouvez le mot ou l'expression grâce aux indices sur le plateau de jeu. "
                            , typography Instructions a [ href "https://www.regledujeu.fr/concept/", target "_blank" ] "Voir les règles de Concept"
                            ]
                        , typography Instructions p [ css [ textAlign center, marginTop Spacing.S ] ] "(Les accents et majuscules ne sont pas importants.)"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , div [ class "images" ] (List.map (\image -> img [ src image, alt "image de l'énigme", class "image" ] []) model.current.images)
                        , form [ onSubmit (Try model.fieldValue), css [ textAlign center ] ]
                            [ input [ type_ "text", value model.fieldValue, onInput FieldChanged ] []
                            , button [ type_ "submit" ] [ text "Valider" ]
                            ]
                        ]

                Done _ ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] ("Défi terminé ! Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 3 ]
                        , p [] [ twitterLink 3 ]
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
                , ( "tries", Encode.int model.tries )
                , ( "remaining", Encode.int (List.length model.remaining + 1) )
                ]

        Done tries ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "tries", Encode.int tries )
                ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "in-progress" ->
                        Decode.map2
                            (\tries remaining ->
                                if remaining == 2 then
                                    InProgress { tries = tries, current = firstEnigma, remaining = otherEnigmas, fieldValue = "" }
                                        |> Decode.succeed

                                else
                                    let
                                        remainingEnigmas =
                                            List.drop (2 - remaining - 1) otherEnigmas
                                    in
                                    case remainingEnigmas of
                                        first :: others ->
                                            InProgress { tries = tries, current = first, remaining = others, fieldValue = "" }
                                                |> Decode.succeed

                                        [] ->
                                            Decode.fail "Invalid state"
                            )
                            (Decode.field "tries" Decode.int)
                            (Decode.field "remaining" Decode.int)
                            |> Decode.andThen identity

                    "done" ->
                        Decode.map Done (Decode.field "tries" Decode.int)

                    stateValue ->
                        Decode.fail ("Unknown state value: " ++ stateValue)
            )


styles : List Snippet
styles =
    [ Css.class "images"
        [ displayFlex
        , flexWrap wrap
        , justifyContent center
        , marginBottom Spacing.M
        ]
    , Css.class "image"
        [ height (px 150)
        , maxWidth (pct 100)
        ]
    ]
