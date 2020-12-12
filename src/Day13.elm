module Day13 exposing (Model, Msg, init, isDone, saveState, stateDecoder, tick, update, view)

import Css exposing (absolute, alignItems, alignSelf, backgroundColor, baseline, batch, block, bold, borderRadius, center, circle, color, column, cursor, display, displayFlex, flexDirection, flexWrap, fontWeight, height, int, justifyContent, left, listStyleType, opacity, pct, pointer, position, relative, rem, right, row, spaceBetween, textAlign, top, width, wrap)
import Css.Global as Css exposing (Snippet)
import DesignSystem.Colors as Colors exposing (green, red, white)
import DesignSystem.Link exposing (homeLink)
import DesignSystem.SocialMedia exposing (facebookLink, twitterLink)
import DesignSystem.Spacing as Spacing exposing (marginBottom, marginTop)
import DesignSystem.Typography exposing (TypographyType(..), fontSize, typography)
import Html.Styled exposing (Html, br, button, div, form, h1, img, input, li, p, span, text, ul)
import Html.Styled.Attributes exposing (alt, class, classList, css, placeholder, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import Pages exposing (images)
import Pages.ImagePath as ImagePath exposing (ImagePath)
import Set exposing (Set)
import String.Normalize exposing (removeDiacritics)
import Time exposing (Posix, Zone)


type Model
    = ReadingInstructions
    | InProgress Game
    | Done { answers : Set String }


type alias Game =
    { startedAt : Posix, now : Posix, input : String, answers : Set String }


type Msg
    = ClickedStart
    | UpdatedInput String
    | Validated


init : Model
init =
    ReadingInstructions


isDone : Model -> Bool
isDone model =
    case model of
        ReadingInstructions ->
            False

        InProgress _ ->
            False

        Done _ ->
            True


maxDuration : number
maxDuration =
    3 * 60 * 1000


tick : Posix -> Model -> Model
tick now model =
    case model of
        ReadingInstructions ->
            model

        InProgress data ->
            if Time.posixToMillis now - Time.posixToMillis data.startedAt > maxDuration then
                Done { answers = data.answers }

            else
                InProgress { data | now = now }

        Done _ ->
            model


update : Posix -> Model -> Msg -> Model
update now model msg =
    case model of
        ReadingInstructions ->
            case msg of
                ClickedStart ->
                    InProgress { startedAt = now, now = now, input = "", answers = Set.empty }

                UpdatedInput _ ->
                    model

                Validated ->
                    model

        InProgress data ->
            case msg of
                ClickedStart ->
                    model

                UpdatedInput input ->
                    InProgress { data | input = input }

                Validated ->
                    let
                        trimed =
                            String.trim data.input
                    in
                    if trimed /= "" then
                        InProgress { data | input = "", answers = Set.insert trimed data.answers }

                    else
                        model

        Done _ ->
            model


view : Zone -> Posix -> Model -> Html Msg
view zone currentDate state =
    let
        maxDay =
            if Time.toYear zone currentDate > 2020 then
                24

            else
                Time.toDay zone currentDate
    in
    if maxDay < 13 then
        typography HeroText p [ css [ textAlign center, marginTop Spacing.XL ] ] "Ce jour n'est pas encore accessible, petit malin ! 😉🎁🎄"

    else
        div []
            [ typography Title1 h1 [ css [ marginTop Spacing.L, marginBottom Spacing.M, textAlign center ] ] "Jour 13 | Boggle"
            , case state of
                ReadingInstructions ->
                    div [ css [ Css.displayFlex, flexDirection column, justifyContent center, alignItems center ] ]
                        [ instructions
                        , button
                            [ onClick ClickedStart, type_ "button", css [ cursor pointer, Spacing.marginTop Spacing.S ] ]
                            [ text "Commencer !" ]
                        ]

                InProgress game ->
                    viewGame (NotEnded game)

                Done data ->
                    viewGame (Ended data)
            , typography Paragraph
                p
                [ css [ marginTop Spacing.L, fontWeight bold, textAlign center ] ]
                "Encore une fois, un énorme merci à Seb qui a créé également l'épreuve d'aujourd'hui ! 🎉"
            , homeLink
            ]


instructions : Html Msg
instructions =
    div []
        [ typography Instructions p [ css [ textAlign center ] ] "Trouver un maximum de mots en formant des chaînes de  lettres contiguës. Plus le mot est long, plus il rapporte de points."
        , typography Instructions p [ css [ textAlign center ] ] "Vous pouvez passer d'une lettre à la suivante située directement à gauche, à droite, en haut, en bas, ou sur l'une des quatre cases diagonales."
        , ul [ css [ listStyleType circle, Spacing.marginLeft Spacing.M, Spacing.marginTop Spacing.S ] ]
            [ typography Instructions li [] "Une lettre ne peut pas être utilisée plus d'une fois pour un même mot."
            , typography Instructions li [] "Seuls les mots de trois lettres ou plus comptent."
            , typography Instructions li [] "Les accents ne sont pas importants. E peut être utilisé comme E, E, E, etc."
            ]
        , div [ css [ displayFlex, flexDirection row, flexWrap wrap, Spacing.marginTop Spacing.S, justifyContent spaceBetween ] ]
            [ img [ src (ImagePath.toString images.day13.example1), alt "exemple de mot au boggle" ] []
            , img [ src (ImagePath.toString images.day13.example2), alt "exemple de mot au boggle" ] []
            , img [ src (ImagePath.toString images.day13.example3), alt "exemple de mot au boggle" ] []
            ]
        ]


type GameState
    = Ended { answers : Set String }
    | NotEnded Game


toAnswers : GameState -> Set String
toAnswers state =
    case state of
        Ended { answers } ->
            answers

        NotEnded { answers } ->
            answers


viewGame : GameState -> Html Msg
viewGame state =
    div [ css [ displayFlex, flexDirection column, alignItems center ] ]
        [ img [ src (ImagePath.toString images.day13.boggle), alt "jeu de boggle", css [ Spacing.marginTop Spacing.S, width (rem 17) ] ] []
        , case state of
            NotEnded game ->
                div [ css [ displayFlex, flexDirection column, alignItems center, Spacing.marginTop Spacing.S ] ]
                    [ form [ onSubmit Validated, css [ textAlign center, Spacing.marginTop Spacing.S ] ]
                        [ input [ type_ "text", value game.input, onInput UpdatedInput, placeholder "Entrer un mot!" ] []
                        , button [ type_ "submit" ] [ text "Valider" ]
                        ]
                    , div [ css [ fontWeight (int 600), color Colors.secondary, DesignSystem.Typography.fontSize DesignSystem.Typography.L ] ]
                        [ text (msToMMSS (maxDuration - Time.posixToMillis game.now + Time.posixToMillis game.startedAt)) ]
                    ]

            Ended _ ->
                div [ css [ textAlign center, marginTop Spacing.S ] ]
                    [ typography HeroText p [ css [ marginTop Spacing.M ] ] "Temps terminé !"
                    , p [ css [ marginTop Spacing.L, marginBottom Spacing.S ] ] [ facebookLink 13 ]
                    , p [] [ twitterLink 13 ]
                    ]
        , viewAnswers (toAnswers state)
        ]


msToMMSS : Int -> String
msToMMSS ms =
    let
        totalSecs =
            ms // 1000

        mins =
            totalSecs // 60

        secs =
            remainderBy 60 totalSecs
    in
    (if mins < 10 then
        "0"

     else
        ""
    )
        ++ String.fromInt mins
        ++ ":"
        ++ (if secs < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt secs


normalize : String -> String
normalize string =
    String.replace " " "" string
        |> removeDiacritics
        |> String.toUpper


normalizeAnswers : Set String -> List String
normalizeAnswers answers =
    Set.toList answers
        |> List.map normalize
        |> List.sortBy (\ans -> ( String.length ans, ans ))


score : String -> Int
score answer =
    let
        n =
            String.length answer
    in
    if n < 3 || not (Set.member answer possibleAnswers) then
        0

    else
        case n of
            3 ->
                1

            4 ->
                1

            5 ->
                2

            6 ->
                3

            7 ->
                5

            8 ->
                11

            _ ->
                11


viewAnswers : Set String -> Html Msg
viewAnswers answers =
    let
        normalizedAnswers =
            normalizeAnswers answers
    in
    div [ css [ displayFlex, flexDirection column, Spacing.marginTop Spacing.S ] ]
        (div [ css [ displayFlex, flexDirection row, alignItems baseline ] ]
            [ div [ css [ width (rem 6), textAlign right ] ] [ text "Total" ]
            , div
                [ css
                    [ fontSize DesignSystem.Typography.L
                    , fontWeight (int 800)
                    , Spacing.marginLeft Spacing.S
                    ]
                ]
                [ text (String.fromInt (List.sum (List.map score normalizedAnswers))) ]
            ]
            :: List.map viewAnswer normalizedAnswers
        )


viewAnswer : String -> Html Msg
viewAnswer answer =
    let
        s =
            score answer
    in
    div [ css [ displayFlex, flexDirection row, alignItems baseline ] ]
        [ div [ css [ width (rem 6), textAlign right, fontWeight (int 600) ] ]
            [ text answer ]
        , div
            [ css
                [ if s == 0 then
                    color Colors.red

                  else
                    color Colors.green
                , fontSize DesignSystem.Typography.L
                , fontWeight (int 800)
                , Spacing.marginLeft Spacing.S
                ]
            ]
            [ text (String.fromInt s) ]
        ]


saveState : Model -> Encode.Value
saveState model =
    case model of
        ReadingInstructions ->
            Encode.object [ ( "state", Encode.string "readingInstructions" ) ]

        InProgress _ ->
            Encode.object [ ( "state", Encode.string "readingInstructions" ) ]

        Done data ->
            Encode.object
                [ ( "state", Encode.string "done" )
                , ( "answers", Encode.list Encode.string (Set.toList data.answers) )
                ]


stateDecoder : Decode.Decoder Model
stateDecoder =
    Decode.field "state" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "readingInstructions" ->
                        Decode.succeed ReadingInstructions

                    "done" ->
                        Decode.field "answers"
                            (Decode.list Decode.string
                                |> Decode.map Set.fromList
                            )
                            |> Decode.map (\a -> Done { answers = a })

                    _ ->
                        Decode.fail ("unknonwn state: " ++ s)
            )


possibleAnswers : Set.Set String
possibleAnswers =
    Set.fromList
        [ "FOUINARDS"
        , "FOUINARD"
        , "RAREFIAI"
        , "REARQUAI"
        , "REFOUINA"
        , "RESAQUAI"
        , "AINARDS"
        , "ARQUIAN"
        , "FAQUINA"
        , "FRAQUAI"
        , "FRASEAU"
        , "FREQUIN"
        , "NIAQUAI"
        , "OUAIRES"
        , "RAREFIA"
        , "REARQUA"
        , "RESAQUA"
        , "AINARD"
        , "ARARES"
        , "ARASER"
        , "ARQUAI"
        , "FAIRES"
        , "FAOUIN"
        , "FAQUIN"
        , "FERIAI"
        , "FIERAS"
        , "FIFRAS"
        , "FIFRES"
        , "FOUINA"
        , "FRAQUA"
        , "NIAFOU"
        , "NIAQUA"
        , "NIQUAI"
        , "OUAIRE"
        , "RAQUAI"
        , "RASEAU"
        , "REFIAI"
        , "REFOUI"
        , "REQUIN"
        , "RIFAIN"
        , "SAQUAI"
        , "SEQUIN"
        , "SERIAI"
        , "AERAS"
        , "AIRAS"
        , "AIRES"
        , "AQUIN"
        , "ARARE"
        , "ARARS"
        , "ARASE"
        , "ARDAS"
        , "AREAU"
        , "ARQUA"
        , "DARAI"
        , "DARSE"
        , "DASRI"
        , "DRAAI"
        , "DRAIN"
        , "EQUIN"
        , "FAIRE"
        , "FERAS"
        , "FERIA"
        , "FIERA"
        , "FIERS"
        , "FIFRA"
        , "FIFRE"
        , "FOUIN"
        , "FRASE"
        , "FREIA"
        , "FRIES"
        , "IAIRA"
        , "NARDS"
        , "NARSE"
        , "NIQUA"
        , "QUINA"
        , "RAARD"
        , "RAQUA"
        , "RARES"
        , "RASER"
        , "REFIA"
        , "REFUI"
        , "RIESA"
        , "SAQUA"
        , "SARAI"
        , "SARAN"
        , "SARIA"
        , "SEIRA"
        , "SERIA"
        , "SERIF"
        , "AARI"
        , "ADRA"
        , "AERA"
        , "AFES"
        , "AIAI"
        , "AIES"
        , "AIRA"
        , "AIRE"
        , "AIRS"
        , "AQUA"
        , "ARAD"
        , "ARAI"
        , "ARAN"
        , "ARAQ"
        , "ARAR"
        , "ARAS"
        , "ARDA"
        , "ARDS"
        , "AREA"
        , "ARES"
        , "ARIA"
        , "ARIF"
        , "DARE"
        , "DARI"
        , "DASR"
        , "DRAN"
        , "ERAD"
        , "ERSA"
        , "ESAR"
        , "FAAA"
        , "FAAS"
        , "FAES"
        , "FEAI"
        , "FEAS"
        , "FERA"
        , "FERS"
        , "FIAI"
        , "FIEF"
        , "FIER"
        , "FIES"
        , "FIFA"
        , "FIFE"
        , "FIFO"
        , "FOUI"
        , "FRAD"
        , "FRAS"
        , "FREQ"
        , "FRIA"
        , "FRIE"
        , "IAAS"
        , "IERS"
        , "IFRS"
        , "IRAQ"
        , "IRAS"
        , "IRES"
        , "NARA"
        , "NARD"
        , "NIAI"
        , "NIAU"
        , "OAAS"
        , "OUAF"
        , "OUAI"
        , "OUFA"
        , "OUIN"
        , "QUAI"
        , "QUIA"
        , "QUIN"
        , "RAAR"
        , "RADS"
        , "RAES"
        , "RAIA"
        , "RAIN"
        , "RANI"
        , "RARA"
        , "RARE"
        , "RASD"
        , "RASE"
        , "READ"
        , "REAI"
        , "REAO"
        , "REAS"
        , "REAU"
        , "RESA"
        , "RIAO"
        , "RIES"
        , "SARA"
        , "SARE"
        , "SARF"
        , "SARI"
        , "SDAR"
        , "SEAU"
        , "SEIA"
        , "SERA"
        , "SERF"
        , "SERI"
        , "SRIF"
        ]
