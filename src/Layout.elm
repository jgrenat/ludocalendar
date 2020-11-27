module Layout exposing (view)

import DesignSystem.Spacing as Spacing exposing (marginTop)
import DesignSystem.Stylesheet exposing (stylesheet)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html
import Html.Styled as Styled exposing (Html, a, div, footer, h1, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, href)
import Metadata exposing (Metadata)
import Pages exposing (pages)
import Pages.PagePath as PagePath exposing (PagePath)


view :
    { title : String, body : List (Html msg) }
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        }
    -> { title : String, body : Html.Html msg }
view document page =
    { title = document.title
    , body =
        div [ class "container" ]
            [ stylesheet
            , header
            , typography HeroText p [ class "catchPhrase" ] "Le calendrier de l'avent de vos jeux de société !"
            , div [ css [ marginTop Spacing.M ] ] document.body
            ]
            |> toUnstyled
    }


header : Html msg
header =
    Styled.header []
        [ a [ href (PagePath.toString pages.index) ]
            [ h1 []
                [ typography MainTitleFirstPart span [] "Ludo"
                , typography MainTitleSecondPart span [] "Calendar"
                ]
            ]
        ]
