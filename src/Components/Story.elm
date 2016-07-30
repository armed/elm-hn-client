module Components.Story exposing (..)

-- vendor
import Html exposing (Html, div, h4, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Html.Attributes.Extra exposing (innerHtml)

-- local
import Model exposing (Item (..), ItemData, runWithDefault)
import Components.Comment as Comment exposing (comments)


view : Maybe Item -> Html a
view mbStory =
  let
    renderStory mbStory =
      case mbStory of
        Just storyItem ->
          runWithDefault storyItem fullStory emptyStory

        Nothing ->
          emptyStory
  in
    div [ class "story" ] <| renderStory mbStory


emptyStory : List (Html a)
emptyStory =
  []


fullStory : ItemData -> List (Html a)
fullStory story =
  [ div [ class "story-body" ]
      [ div [ class "story-text", innerHtml story.text ] []
      , div [ class "story-comments" ]
          <| comments story.kids
      ]
  ]
