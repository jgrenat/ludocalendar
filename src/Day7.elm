module Day7 exposing (Model, Msg, init, isDone, saveState, stateDecoder, update, view)

import Css exposing (absolute, alignItems, backgroundColor, block, bold, border, center, cursor, display, displayFlex, flexWrap, fontWeight, int, justifyContent, left, maxHeight, maxWidth, notAllowed, pct, pointer, position, px, relative, rgba, right, spaceAround, textAlign, top, transparent, width, wrap, zIndex, zero)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (SpacingSize(..), marginBottom, marginTop, padding, paddingTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, button, div, h1, img, p)
import Html.Styled.Attributes exposing (alt, class, css, disabled, src, type_)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import Time exposing (Posix, Zone)


type alias Enigma =
    { question : String
    , cluesImages : List String
    , firstImage : String
    , secondImage : String
    , thirdImage : String
    , answer : String
    }


characterChoice : Enigma
characterChoice =
    { question = "Quel personnage dans la liste ci-dessous est le coupable ?"
    , cluesImages = [ ImagePath.toString images.day7.clue4, ImagePath.toString images.day7.clue7 ]
    , firstImage = ImagePath.toString images.day7.character1
    , secondImage = ImagePath.toString images.day7.character2
    , thirdImage = ImagePath.toString images.day7.character3
    , answer = ImagePath.toString images.day7.character2
    }


placeChoice : Enigma
placeChoice =
    { question = "Dans quel lieu s'est déroulé le crime ?"
    , cluesImages = [ ImagePath.toString images.day7.clue1, ImagePath.toString images.day7.clue12, ImagePath.toString images.day7.clue3 ]
    , firstImage = ImagePath.toString images.day7.place1
    , secondImage = ImagePath.toString images.day7.place2
    , thirdImage = ImagePath.toString images.day7.place3
    , answer = ImagePath.toString images.day7.place3
    }


weaponChoice : Enigma
weaponChoice =
    { question = "Quel objet a été renversé ?"
    , cluesImages = [ ImagePath.toString images.day7.clue6 ]
    , firstImage = ImagePath.toString images.day7.weapon1
    , secondImage = ImagePath.toString images.day7.weapon2
    , thirdImage = ImagePath.toString images.day7.weapon3
    , answer = ImagePath.toString images.day7.weapon3
    }


type EnigmaState
    = FirstTry
    | SecondTry String
    | ThirdTry String String


type Model
    = GuessCharacter EnigmaState
    | GuessPlace Int EnigmaState
    | GuessWeapon Int Int EnigmaState
    | Done Int


init : Model
init =
    GuessCharacter FirstTry


type Msg
    = Try String


update : Model -> Msg -> Model
update modelState msg =
    case modelState of
        GuessCharacter state ->
            case updateState characterChoice state msg of
                SameEnigma newState ->
                    GuessCharacter newState

                NextEnigma score ->
                    GuessPlace score FirstTry

        GuessPlace charactersScore state ->
            case updateState placeChoice state msg of
                SameEnigma newState ->
                    GuessPlace charactersScore newState

                NextEnigma score ->
                    GuessWeapon charactersScore score FirstTry

        GuessWeapon charactersScore placesScore state ->
            case updateState weaponChoice state msg of
                SameEnigma newState ->
                    GuessWeapon charactersScore placesScore newState

                NextEnigma score ->
                    Done (charactersScore + placesScore + score)

        Done score ->
            Done score


type UpdateResult
    = SameEnigma EnigmaState
    | NextEnigma Int


updateState : Enigma -> EnigmaState -> Msg -> UpdateResult
updateState enigma state msg =
    case ( state, msg ) of
        ( FirstTry, Try answer ) ->
            if answer == enigma.answer then
                NextEnigma 8

            else
                SameEnigma (SecondTry answer)

        ( SecondTry firstWrongAnswer, Try answer ) ->
            if answer == enigma.answer then
                NextEnigma 4

            else
                SameEnigma (ThirdTry firstWrongAnswer answer)

        ( ThirdTry firstWrongAnswer secondWrongAnswer, Try answer ) ->
            if answer == enigma.answer then
                NextEnigma 1

            else
                SameEnigma (ThirdTry firstWrongAnswer secondWrongAnswer)


getScore : Model -> Int
getScore state =
    case state of
        Done score ->
            score

        GuessCharacter _ ->
            0

        GuessPlace charactersScore _ ->
            charactersScore

        GuessWeapon charactersScore placesScore _ ->
            charactersScore + placesScore


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay < 7 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div [ class "day4" ]
            [ Css.global styles
            , typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 7 | Mysterium"
            , case state of
                GuessCharacter enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Vous étiez tranquillement en train de décorer votre manoir pour Noël quand soudain, un grand fracas retentit dans l'une des pièces du bas ! Quelqu'un a dû renverser ou casser quelque chose..."
                        , typography Instructions p [ css [ textAlign center ] ] "Heureusement, vous êtes un medium émerite et un fantôme a tout vu ! Il vous envoie des images de l'Au-delà pour que vous puissiez identifier le coupable, la pièce où s'est produit le drame et l'objet renversé ! C'est un peu cryptique, mais ça ne devrait pas vous inquiéter, vu votre expertise !"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.S ] ] "Ses premiers indices sont les suivants : "
                        , div [ class "clues" ] (List.map viewClue characterChoice.cluesImages)
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.S, marginTop Spacing.L ] ] characterChoice.question
                        , viewAnswerImages characterChoice enigmaState
                        ]

                GuessPlace _ enigmaState ->
                    div []
                        [ typography Instructions p [ css [ textAlign center ] ] "Grâce à votre ami fantôme, vous avez trouvé le filou en question ! Mais où a-t-il renversé un objet ?"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.S ] ] "Les indices du fantôme sont les suivants : "
                        , div [ class "clues" ] (List.map viewClue placeChoice.cluesImages)
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.S, marginTop Spacing.L ] ] placeChoice.question
                        , viewAnswerImages placeChoice enigmaState
                        ]

                GuessWeapon _ _ enigmaState ->
                    div [ class "weapon-choice" ]
                        [ typography Instructions p [ css [ textAlign center ] ] "Cela s'est effectivement passé dans la cuisine... Mais quel était l'objet renversé ?"
                        , typography Paragraph p [ css [ textAlign right, marginTop Spacing.S, marginBottom Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.S ] ] "L'indice du fantôme est le suivant : "
                        , div [ class "clues" ] (List.map viewClue weaponChoice.cluesImages)
                        , typography HeroText p [ css [ textAlign center, marginBottom Spacing.S, marginTop Spacing.L ] ] weaponChoice.question
                        , viewAnswerImages weaponChoice enigmaState
                        ]

                Done _ ->
                    div [ css [ textAlign center, marginTop Spacing.XL ] ]
                        [ typography HeroText p [] "Grâce au fantôme, vous avez pu lever le mystère sur cette affaire !"
                        , typography HeroText p [ css [ marginTop Spacing.M ] ] ("Votre score : " ++ String.fromInt (getScore state))
                        , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 7 ]
                        , p [] [ twitterLink 7 ]
                        ]
            , typography Paragraph p [ css [ marginTop Spacing.L, fontWeight bold, textAlign center ] ] "Un énorme merci à Zoé qui a fourni les photos d'aujourd'hui ! 🎉"
            , homeLink
            ]


viewClue : String -> Html msg
viewClue imageUrl =
    img [ src imageUrl, alt "image de l'énigme" ] []


viewAnswerImages : Enigma -> EnigmaState -> Html Msg
viewAnswerImages enigma enigmaState =
    case enigmaState of
        FirstTry ->
            div [ class "answerImages" ]
                [ viewUnselectedAnswerImage enigma.firstImage
                , viewUnselectedAnswerImage enigma.secondImage
                , viewUnselectedAnswerImage enigma.thirdImage
                ]

        SecondTry firstWrongAnswer ->
            div [ class "answerImages" ]
                [ if enigma.firstImage == firstWrongAnswer then
                    viewWrongAnswerImage enigma.firstImage

                  else
                    viewUnselectedAnswerImage enigma.firstImage
                , if enigma.secondImage == firstWrongAnswer then
                    viewWrongAnswerImage enigma.secondImage

                  else
                    viewUnselectedAnswerImage enigma.secondImage
                , if enigma.thirdImage == firstWrongAnswer then
                    viewWrongAnswerImage enigma.thirdImage

                  else
                    viewUnselectedAnswerImage enigma.thirdImage
                ]

        ThirdTry firstWrongAnswer secondWrongAnswer ->
            div [ class "answerImages" ]
                [ if enigma.firstImage == firstWrongAnswer || enigma.firstImage == secondWrongAnswer then
                    viewWrongAnswerImage enigma.firstImage

                  else
                    viewUnselectedAnswerImage enigma.firstImage
                , if enigma.secondImage == firstWrongAnswer || enigma.secondImage == secondWrongAnswer then
                    viewWrongAnswerImage enigma.secondImage

                  else
                    viewUnselectedAnswerImage enigma.secondImage
                , if enigma.thirdImage == firstWrongAnswer || enigma.thirdImage == secondWrongAnswer then
                    viewWrongAnswerImage enigma.thirdImage

                  else
                    viewUnselectedAnswerImage enigma.thirdImage
                ]


viewUnselectedAnswerImage : String -> Html Msg
viewUnselectedAnswerImage imageUrl =
    button [ type_ "button", onClick (Try imageUrl) ] [ img [ src imageUrl, alt "image de l'énigme" ] [] ]


viewWrongAnswerImage : String -> Html msg
viewWrongAnswerImage imageUrl =
    button [ type_ "button", disabled True, class "imageAnswer--incorrect" ]
        [ img [ src imageUrl, alt "image de l'énigme" ] []
        ]


isDone : Model -> Bool
isDone model =
    case model of
        Done _ ->
            True

        _ ->
            False


saveState : Model -> Encode.Value
saveState modelState =
    case modelState of
        GuessCharacter enigmaState ->
            Encode.object
                [ ( "state", Encode.string "guess-character" )
                , ( "enigma-state", encodeEnigmaState enigmaState )
                ]

        GuessPlace charactersScore enigmaState ->
            Encode.object
                [ ( "state", Encode.string "guess-place" )
                , ( "characters-score", Encode.int charactersScore )
                , ( "enigma-state", encodeEnigmaState enigmaState )
                ]

        GuessWeapon charactersScore placesScore enigmaState ->
            Encode.object
                [ ( "state", Encode.string "guess-weapon" )
                , ( "characters-score", Encode.int charactersScore )
                , ( "places-score", Encode.int placesScore )
                , ( "enigma-state", encodeEnigmaState enigmaState )
                ]

        Done score ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "score", Encode.int score )
                ]


encodeEnigmaState : EnigmaState -> Encode.Value
encodeEnigmaState enigmaState =
    case enigmaState of
        FirstTry ->
            Encode.object [ ( "step", Encode.string "first-try" ) ]

        SecondTry firstWrongAnswer ->
            Encode.object
                [ ( "step", Encode.string "second-try" )
                , ( "first-wrong-answer", Encode.string firstWrongAnswer )
                ]

        ThirdTry firstWrongAnswer secondWrongAnswer ->
            Encode.object
                [ ( "step", Encode.string "third-try" )
                , ( "first-wrong-answer", Encode.string firstWrongAnswer )
                , ( "second-wrong-answer", Encode.string secondWrongAnswer )
                ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\state ->
                case state of
                    "guess-character" ->
                        Decode.map GuessCharacter (Decode.field "enigma-state" enigmaStateDecoder)

                    "guess-place" ->
                        Decode.map2 GuessPlace
                            (Decode.field "characters-score" Decode.int)
                            (Decode.field "enigma-state" enigmaStateDecoder)

                    "guess-weapon" ->
                        Decode.map3 GuessWeapon
                            (Decode.field "characters-score" Decode.int)
                            (Decode.field "places-score" Decode.int)
                            (Decode.field "enigma-state" enigmaStateDecoder)

                    "done" ->
                        Decode.map Done (Decode.field "score" Decode.int)

                    _ ->
                        Decode.fail "Invalid state"
            )


enigmaStateDecoder : Decoder EnigmaState
enigmaStateDecoder =
    Decode.field "step" Decode.string
        |> Decode.andThen
            (\step ->
                case step of
                    "first-try" ->
                        Decode.succeed FirstTry

                    "second-try" ->
                        Decode.map SecondTry (Decode.field "first-wrong-answer" Decode.string)

                    "third-try" ->
                        Decode.map2 ThirdTry
                            (Decode.field "first-wrong-answer" Decode.string)
                            (Decode.field "second-wrong-answer" Decode.string)

                    _ ->
                        Decode.fail ("Invalid step: " ++ step)
            )


styles : List Snippet
styles =
    [ Css.class "clues"
        [ displayFlex
        , alignItems center
        , justifyContent spaceAround
        , flexWrap wrap
        , Css.children
            [ Css.img
                [ maxWidth (px 220)
                , width (pct 100)
                , paddingTop Spacing.M
                ]
            ]
        ]
    , Css.class "answerImages"
        [ displayFlex
        , alignItems center
        , justifyContent spaceAround
        , flexWrap wrap
        , Css.children
            [ Css.button
                [ width (pct 100)
                , maxWidth (px 390)
                , marginTop Spacing.S
                , cursor pointer
                , padding Spacing.NoSpace
                , border zero
                , backgroundColor transparent
                , position relative
                , Css.children
                    [ Css.img
                        [ width (pct 100) ]
                    ]
                , Css.hover
                    [ Css.after
                        [ Css.property "content" "''"
                        , position absolute
                        , display block
                        , left zero
                        , top zero
                        , Css.backgroundColor (rgba 250 250 250 0.3)
                        , Css.width (pct 100)
                        , Css.height (pct 100)
                        , zIndex (int 1)
                        ]
                    ]
                , Css.withClass "imageAnswer--incorrect"
                    [ cursor notAllowed
                    , Css.after
                        [ Css.property "content" "''"
                        , position absolute
                        , display block
                        , left zero
                        , top zero
                        , Css.backgroundColor (rgba 250 0 0 0.3)
                        , Css.width (pct 100)
                        , Css.height (pct 100)
                        , zIndex (int 1)
                        ]
                    ]
                ]
            ]
        ]
    , Css.class "weapon-choice"
        [ Css.descendants
            [ Css.class "answerImages"
                [ Css.descendants
                    [ Css.button
                        [ maxWidth (px 230)
                        ]
                    ]
                ]
            ]
        ]
    ]
