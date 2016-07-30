module Components.Header exposing (..)

import Html exposing (Html, div, text, a, span)
import Html.Attributes exposing (class, href, title, target)
import Html.Events exposing (onClick)
import Msg exposing (Msg (CloseStory))
import Model exposing (Item, runWithDefault)


view : Maybe Item -> Html Msg
view mbStory =
  let
    (isStoryOpened, titleText) =
      case mbStory of
        Just story ->
          (True, runWithDefault story .title "")

        Nothing ->
          (False, "")

    clz = if isStoryOpened then
            " story-opened"
          else
            ""
  in
  div [ class <| "header" ++ clz ]
    [ githubLink
    , span [ class "logo" ] []
    , span [ class "back-btn", onClick CloseStory ] []
    , div [ class "title" ]
        [ text titleText ]
    ]


githubLink : Html a
githubLink =
  a [ class "github-link"
    , href "https://github.com/armed/elm-hn-client"
    , title "Open Github repo"
    , target "_blank"
    ] []
