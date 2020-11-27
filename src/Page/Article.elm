module Page.Article exposing (view)

import Data.Author as Author
import Date exposing (Date)
import Html.Styled exposing (Html, div, h2, img, text)
import Html.Styled.Attributes exposing (alt, src)
import Metadata exposing (ArticleMetadata)
import Pages
import Pages.ImagePath as ImagePath exposing (ImagePath)


view : ArticleMetadata -> Html msg -> { title : String, body : List (Html msg) }
view metadata viewForPage =
    { title = metadata.title
    , body =
        [ div []
            [ div []
                [ Author.view [] metadata.author
                , text metadata.author.name
                , text metadata.author.bio
                ]
            ]
        , div [] [ publishedDateView metadata ]
        , h2 [] [ text metadata.title ]
        , articleImageView metadata.image
        , viewForPage
        ]
    }


publishedDateView : { a | published : Date } -> Html msg
publishedDateView metadata =
    text
        (metadata.published
            |> Date.format "MMMM ddd, yyyy"
        )


articleImageView : ImagePath Pages.PathKey -> Html msg
articleImageView articleImage =
    img [ src (ImagePath.toString articleImage), alt "Article cover photo" ] []
