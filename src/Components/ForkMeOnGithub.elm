module Components.ForkMeOnGithub exposing (forkMeOnGithub)

import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href, title, target)


forkMeOnGithub : Html a
forkMeOnGithub =
  a [ class "github-link"
    , href "https://github.com/armed/elm-hn-client"
    , title "Open Github repo"
    , target "_blank"
    ] []
