module DesignSystem.Link exposing (homeLink)

import Css exposing (center, textAlign)
import DesignSystem.Spacing as Spacing exposing (marginTop)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import Html.Styled exposing (Html, a, p)
import Html.Styled.Attributes exposing (css, href)
import Pages exposing (pages)
import Pages.PagePath as PagePath


homeLink : Html msg
homeLink =
    p [ css [ marginTop Spacing.L, textAlign center ] ] [ typography Paragraph a [ href (PagePath.toString pages.index) ] "< Retour à l'accueil" ]
