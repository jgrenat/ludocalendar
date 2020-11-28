module Main exposing (main)

import Color
import Day1
import Day2
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
    , description = "LudoCalendar - A statically typed site generator."
    , iarcRatingId = Nothing
    , name = "elm-pages-starter"
    , themeColor = Just Color.white
    , startUrl = pages.index
    , shortName = Just "elm-pages-starter"
    , sourceIcon = images.iconPng
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
    ( Model Time.utc (millisToPosix 0) Day1.init Day2.init
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            ( { model | currentDate = posix }, Cmd.none )

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


subscriptions _ _ _ =
    Sub.batch [ Time.every (60 * 1000) Tick, stateFromLocalStorage StateLoaded ]


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
                            { url = images.iconPng
                            , alt = "LudoCalendar logo"
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
                            { url = images.iconPng
                            , alt = "LudoCalendar logo"
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
                            { url = images.iconPng
                            , alt = "LudoCalendar logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "Deuxième jour"
                        }
                        |> Seo.website
           )


canonicalSiteUrl : String
canonicalSiteUrl =
    "https://ludocalendar.com"


siteTagline : String
siteTagline =
    "Le calendrier de l'avent de vos jeux de société !"


saveDay : Int -> (model -> Encode.Value) -> model -> Cmd msg
saveDay day encoder model =
    saveToLocalStorage (Ports.DayState day (encoder model))


stateDecoder : Zone -> Posix -> Decoder Model
stateDecoder zone time =
    Decode.map2 (Model zone time)
        (Decode.oneOf [ Decode.field "day1" Day1.stateDecoder, Decode.succeed Day1.init ])
        (Decode.oneOf [ Decode.field "day2" Day2.stateDecoder, Decode.succeed Day2.init ])
