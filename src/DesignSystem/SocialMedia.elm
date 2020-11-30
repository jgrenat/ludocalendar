module DesignSystem.SocialMedia exposing (facebookLink, twitterLink)

import Css exposing (alignItems, center, display, displayFlex, inlineFlex, justifyContent)
import DesignSystem.Typography exposing (TypographyType(..), typography)
import FeatherIcons
import Html.Styled exposing (Html, a, fromUnstyled, span)
import Html.Styled.Attributes exposing (css, href, rel, target)
import Pages exposing (images)
import Pages.ImagePath as ImagePath
import Url


twitterLink : Int -> Html msg
twitterLink day =
    a
        [ rel "nofollow"
        , href ("https://twitter.com/intent/tweet?text=%F0%9F%8E%84%F0%9F%8E%81%20J%27ai%20r%C3%A9ussi%20l%27%C3%A9preuve%20du%20jour%20" ++ String.fromInt day ++ "%20de%20LudoCalendar%2C%20le%20calendrier%20de%20l%27Avent%20des%20jeux%20de%20soci%C3%A9t%C3%A9%20!%0Ahttps%3A%2F%2Fludocalendar.com")
        , target "_blank"
        , css [ display inlineFlex, alignItems center ]
        ]
        [ FeatherIcons.twitter
            |> FeatherIcons.toHtml []
            |> fromUnstyled
        , typography Paragraph span [] " Partager sur Twitter"
        ]


facebookLink : Int -> Html msg
facebookLink day =
    a
        [ rel "nofollow"
        , href "https://www.facebook.com/sharer/sharer.php?u=https%3A//ludocalendar.com"
        , target "_blank"
        , css [ display inlineFlex, alignItems center ]
        ]
        [ FeatherIcons.facebook
            |> FeatherIcons.toHtml []
            |> fromUnstyled
        , typography Paragraph span [] " Partager sur Facebook"
        ]
