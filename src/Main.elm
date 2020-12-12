module Main exposing (main)

import Color
import Day1
import Day10
import Day11
import Day12
import Day13
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Feed
import Head
import Head.Seo as Seo
import Home
import Html
import Html.Styled exposing (Html, fromUnstyled)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Layout
import Markdown.Parser
import Markdown.Renderer
import Metadata exposing (Metadata)
import Model exposing (Model)
import MySitemap
import Pages exposing (images, pages)
import Pages.Manifest as Manifest
import Pages.Manifest.Category
import Pages.PagePath exposing (PagePath)
import Pages.Platform
import Pages.StaticHttp as StaticHttp
import Ports exposing (saveToLocalStorage, stateFromLocalStorage)
import Task
import Time exposing (Posix, Zone, millisToPosix)


manifest : Manifest.Config Pages.PathKey
manifest =
    { backgroundColor = Just Color.white
    , categories = [ Pages.Manifest.Category.entertainment ]
    , displayMode = Manifest.Standalone
    , orientation = Manifest.Portrait
    , description = "LudoCalendar - Le calendrier de l\\'Avent de vos jeux de société"
    , iarcRatingId = Nothing
    , name = "LudoCalendar"
    , themeColor = Just Color.white
    , startUrl = pages.index
    , shortName = Just "LudoCalendar"
    , sourceIcon = images.androidChrome512x512
    , icons = []
    }


type alias Rendered =
    Html Msg



-- the intellij-elm plugin doesn't support type aliases for Programs so we need to use this line
-- main : Platform.Program Pages.Platform.Flags (Pages.Platform.Model Model Msg Metadata Rendered) (Pages.Platform.Msg Msg Metadata Rendered)


main : Pages.Platform.Program Model Msg Metadata Rendered Pages.PathKey
main =
    Pages.Platform.init
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , documents = [ markdownDocument ]
        , manifest = manifest
        , canonicalSiteUrl = canonicalSiteUrl
        , onPageChange = Nothing
        , internals = Pages.internals
        }
        |> Pages.Platform.withFileGenerator generateFiles
        |> Pages.Platform.toProgram


generateFiles :
    List
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        , body : String
        }
    ->
        StaticHttp.Request
            (List
                (Result String
                    { path : List String
                    , content : String
                    }
                )
            )
generateFiles siteMetadata =
    StaticHttp.succeed
        [ Feed.fileToGenerate { siteTagline = siteTagline, siteUrl = canonicalSiteUrl } siteMetadata |> Ok
        , MySitemap.build { siteUrl = canonicalSiteUrl } siteMetadata |> Ok
        ]


markdownDocument : { extension : String, metadata : Decode.Decoder Metadata, body : String -> Result error Rendered }
markdownDocument =
    { extension = "md"
    , metadata = Metadata.decoder
    , body =
        \markdownBody ->
            Markdown.Parser.parse markdownBody
                |> Result.withDefault []
                |> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer
                |> Result.withDefault [ Html.text "" ]
                |> Html.div []
                |> fromUnstyled
                |> Ok
    }


init : ( Model, Cmd Msg )
init =
    ( Model Time.utc
        (millisToPosix 0)
        Day1.init
        Day2.init
        Day3.init
        Day4.init
        Day5.init
        Day6.init
        Day7.init
        Day8.init
        Day9.init
        Day10.init
        Day11.init
        Day12.init
        Day13.init
    , Cmd.batch
        [ Task.perform ZoneRetrieved Time.here
        , Task.perform Tick Time.now
        ]
    )


type Msg
    = Tick Posix
    | ZoneRetrieved Zone
    | StateLoaded Decode.Value
    | Day1Msg Day1.Msg
    | Day2Msg Day2.Msg
    | Day3Msg Day3.Msg
    | Day4Msg Day4.Msg
    | Day5Msg Day5.Msg
    | Day6Msg Day6.Msg
    | Day7Msg Day7.Msg
    | Day8Msg Day8.Msg
    | Day9Msg Day9.Msg
    | Day10Msg Day10.Msg
    | Day11Msg Day11.Msg
    | Day12Msg Day12.Msg
    | Day13Msg Day13.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            ( { model
                | currentDate = posix
                , day13 = Day13.tick posix model.day13
              }
            , Cmd.none
            )

        ZoneRetrieved zone ->
            ( { model | zone = zone }, Cmd.none )

        StateLoaded value ->
            let
                loadedModelResult =
                    Decode.decodeValue (stateDecoder model.zone model.currentDate) value
            in
            case loadedModelResult of
                Ok loadedModel ->
                    ( loadedModel, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Day1Msg day1Msg ->
            let
                newModel =
                    Day1.update model.day1 day1Msg
            in
            ( { model | day1 = newModel }, saveDay 1 Day1.saveState newModel )

        Day2Msg day2Msg ->
            let
                newModel =
                    Day2.update model.day2 day2Msg
            in
            ( { model | day2 = newModel }, saveDay 2 Day2.saveState newModel )

        Day3Msg day3Msg ->
            let
                newModel =
                    Day3.update model.day3 day3Msg
            in
            ( { model | day3 = newModel }, saveDay 3 Day3.saveState newModel )

        Day4Msg day4Msg ->
            let
                newModel =
                    Day4.update model.day4 day4Msg
            in
            ( { model | day4 = newModel }, saveDay 4 Day4.saveState newModel )

        Day5Msg day5Msg ->
            let
                newModel =
                    Day5.update model.day5 day5Msg
            in
            ( { model | day5 = newModel }, saveDay 5 Day5.saveState newModel )

        Day6Msg day6Msg ->
            let
                newModel =
                    Day6.update model.day6 day6Msg
            in
            ( { model | day6 = newModel }, saveDay 6 Day6.saveState newModel )

        Day7Msg day7Msg ->
            let
                newModel =
                    Day7.update model.day7 day7Msg
            in
            ( { model | day7 = newModel }, saveDay 7 Day7.saveState newModel )

        Day8Msg day8Msg ->
            let
                newModel =
                    Day8.update model.currentDate model.day8 day8Msg
            in
            ( { model | day8 = newModel }, saveDay 8 Day8.saveState newModel )

        Day9Msg day9Msg ->
            let
                newModel =
                    Day9.update model.day9 day9Msg
            in
            ( { model | day9 = newModel }, saveDay 9 Day9.saveState newModel )

        Day10Msg day10Msg ->
            let
                newModel =
                    Day10.update model.day10 day10Msg
            in
            ( { model | day10 = newModel }, saveDay 10 Day10.saveState newModel )

        Day11Msg day11Msg ->
            let
                newModel =
                    Day11.update model.day11 day11Msg
            in
            ( { model | day11 = newModel }, saveDay 11 Day11.saveState newModel )

        Day12Msg day12Msg ->
            let
                newModel =
                    Day12.update model.day12 day12Msg
            in
            ( { model | day12 = newModel }, saveDay 12 Day12.saveState newModel )

        Day13Msg day13Msg ->
            let
                newModel =
                    Day13.update model.currentDate model.day13 day13Msg
            in
            ( { model | day13 = newModel }, saveDay 13 Day13.saveState newModel )


subscriptions metadata _ model =
    let
        daySubscriptions =
            case metadata of
                Metadata.Day11 ->
                    Day11.subscriptions model.day11
                        |> Sub.map Day11Msg

                Metadata.Day12 ->
                    Day12.subscriptions model.day12
                        |> Sub.map Day12Msg

                _ ->
                    Sub.none
    in
    Sub.batch [ Time.every 1000 Tick, stateFromLocalStorage StateLoaded, daySubscriptions ]


view :
    List ( PagePath Pages.PathKey, Metadata )
    -> { path : PagePath Pages.PathKey, frontmatter : Metadata }
    -> StaticHttp.Request { view : Model -> Rendered -> { title : String, body : Html.Html Msg }, head : List (Head.Tag Pages.PathKey) }
view siteMetadata page =
    StaticHttp.succeed
        { view =
            \model viewForPage ->
                Layout.view (pageView model siteMetadata page viewForPage) page
        , head = head page.frontmatter
        }


pageView :
    Model
    -> List ( PagePath Pages.PathKey, Metadata )
    -> { path : PagePath Pages.PathKey, frontmatter : Metadata }
    -> Rendered
    -> { title : String, body : List Rendered }
pageView model siteMetadata page viewForPage =
    case page.frontmatter of
        Metadata.Home ->
            { title = "LudoCalendar"
            , body =
                [ Home.view model
                ]
            }

        Metadata.Day1 ->
            { title = "LudoCalendar – Premier jour"
            , body =
                [ Day1.view model.zone model.currentDate model.day1
                    |> Html.Styled.map Day1Msg
                ]
            }

        Metadata.Day2 ->
            { title = "LudoCalendar – Deuxième jour"
            , body =
                [ Day2.view model.zone model.currentDate model.day2
                    |> Html.Styled.map Day2Msg
                ]
            }

        Metadata.Day3 ->
            { title = "LudoCalendar – Troisième jour"
            , body =
                [ Day3.view model.zone model.currentDate model.day3
                    |> Html.Styled.map Day3Msg
                ]
            }

        Metadata.Day4 ->
            { title = "LudoCalendar – Quatrième jour"
            , body =
                [ Day4.view model.zone model.currentDate model.day4
                    |> Html.Styled.map Day4Msg
                ]
            }

        Metadata.Day5 ->
            { title = "LudoCalendar – Cinquième jour"
            , body =
                [ Day5.view model.zone model.currentDate model.day5
                    |> Html.Styled.map Day5Msg
                ]
            }

        Metadata.Day6 ->
            { title = "LudoCalendar – Sixième jour"
            , body =
                [ Day6.view model.zone model.currentDate model.day6
                    |> Html.Styled.map Day6Msg
                ]
            }

        Metadata.Day7 ->
            { title = "LudoCalendar – Septième jour"
            , body =
                [ Day7.view model.zone model.currentDate model.day7
                    |> Html.Styled.map Day7Msg
                ]
            }

        Metadata.Day8 ->
            { title = "LudoCalendar – Huitième jour"
            , body =
                [ Day8.view model.zone model.currentDate model.day8
                    |> Html.Styled.map Day8Msg
                ]
            }

        Metadata.Day9 ->
            { title = "LudoCalendar – Neuvième jour"
            , body =
                [ Day9.view model.zone model.currentDate model.day9
                    |> Html.Styled.map Day9Msg
                ]
            }

        Metadata.Day10 ->
            { title = "LudoCalendar – Dixième jour"
            , body =
                [ Day10.view model.zone model.currentDate model.day10
                    |> Html.Styled.map Day10Msg
                ]
            }

        Metadata.Day11 ->
            { title = "LudoCalendar – Onzième jour"
            , body =
                [ Day11.view model.zone model.currentDate model.day11
                    |> Html.Styled.map Day11Msg
                ]
            }

        Metadata.Day12 ->
            { title = "LudoCalendar – Douxième jour"
            , body =
                [ Day12.view model.zone model.currentDate model.day12
                    |> Html.Styled.map Day12Msg
                ]
            }

        Metadata.Day13 ->
            { title = "LudoCalendar – Douxième jour"
            , body =
                [ Day13.view model.zone model.currentDate model.day13
                    |> Html.Styled.map Day13Msg
                ]
            }


commonHeadTags : List (Head.Tag Pages.PathKey)
commonHeadTags =
    [ Head.rssLink "/blog/feed.xml"
    , Head.sitemapLink "/sitemap.xml"
    ]



{- Read more about the metadata specs:

   <https://developer.twitter.com/en/docs/tweets/optimize-with-cards/overview/abouts-cards>
   <https://htmlhead.dev>
   <https://html.spec.whatwg.org/multipage/semantics.html#standard-metadata-names>
   <https://ogp.me/>
-}


head : Metadata -> List (Head.Tag Pages.PathKey)
head metadata =
    commonHeadTags
        ++ (case metadata of
                Metadata.Home ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Accueil"
                        }
                        |> Seo.website

                Metadata.Day1 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Premier jour"
                        }
                        |> Seo.website

                Metadata.Day2 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Deuxième jour"
                        }
                        |> Seo.website

                Metadata.Day3 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Troisième jour"
                        }
                        |> Seo.website

                Metadata.Day4 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Quatrième jour"
                        }
                        |> Seo.website

                Metadata.Day5 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Cinquième jour"
                        }
                        |> Seo.website

                Metadata.Day6 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Sixième jour"
                        }
                        |> Seo.website

                Metadata.Day7 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Septième jour"
                        }
                        |> Seo.website

                Metadata.Day8 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Huitième jour"
                        }
                        |> Seo.website

                Metadata.Day9 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Neuvième jour"
                        }
                        |> Seo.website

                Metadata.Day10 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Dixième jour"
                        }
                        |> Seo.website

                Metadata.Day11 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Onzième jour"
                        }
                        |> Seo.website

                Metadata.Day12 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Douxième jour"
                        }
                        |> Seo.website

                Metadata.Day13 ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "LudoCalendar"
                        , image =
                            { url = images.screenshot
                            , alt = "LudoCalendar"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Treizième jour"
                        }
                        |> Seo.website
           )


canonicalSiteUrl : String
canonicalSiteUrl =
    "https://ludocalendar.com"


siteTagline : String
siteTagline =
    "Le calendrier de l'Avent de vos jeux de société !"


saveDay : Int -> (model -> Encode.Value) -> model -> Cmd msg
saveDay day encoder model =
    saveToLocalStorage (Ports.DayState day (encoder model))


andWith : Decoder a -> Decoder (a -> b) -> Decoder b
andWith =
    Decode.map2 (|>)


stateDecoder : Zone -> Posix -> Decoder Model
stateDecoder zone time =
    Decode.succeed (Model zone time)
        |> andWith (Decode.oneOf [ Decode.field "day1" Day1.stateDecoder, Decode.succeed Day1.init ])
        |> andWith (Decode.oneOf [ Decode.field "day2" Day2.stateDecoder, Decode.succeed Day2.init ])
        |> andWith (Decode.oneOf [ Decode.field "day3" Day3.stateDecoder, Decode.succeed Day3.init ])
        |> andWith (Decode.oneOf [ Decode.field "day4" Day4.stateDecoder, Decode.succeed Day4.init ])
        |> andWith (Decode.oneOf [ Decode.field "day5" Day5.stateDecoder, Decode.succeed Day5.init ])
        |> andWith (Decode.oneOf [ Decode.field "day6" Day6.stateDecoder, Decode.succeed Day6.init ])
        |> andWith (Decode.oneOf [ Decode.field "day7" Day7.stateDecoder, Decode.succeed Day7.init ])
        |> andWith (Decode.oneOf [ Decode.field "day8" Day8.stateDecoder, Decode.succeed Day8.init ])
        |> andWith (Decode.oneOf [ Decode.field "day9" Day9.stateDecoder, Decode.succeed Day9.init ])
        |> andWith (Decode.oneOf [ Decode.field "day10" Day10.stateDecoder, Decode.succeed Day10.init ])
        |> andWith (Decode.oneOf [ Decode.field "day11" Day11.stateDecoder, Decode.succeed Day11.init ])
        |> andWith (Decode.oneOf [ Decode.field "day12" Day12.stateDecoder, Decode.succeed Day12.init ])
        |> andWith (Decode.oneOf [ Decode.field "day13" Day13.stateDecoder, Decode.succeed Day13.init ])
