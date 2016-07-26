module Components.StoryLink exposing (view, Msg (..))

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Model exposing (Item (..), ItemData, itemId)


type Msg = Open Int


view : Item -> Maybe Item -> Html Msg
view story mbOpenedStory =
  case story of
    Lite _ ->
      placeholderView

    Full storyData ->
      storyView storyData <| isActive story mbOpenedStory


isActive : Item -> Maybe Item -> Bool
isActive story mbOpenedStory =
  case mbOpenedStory of
    Just openedStory ->
      itemId story == itemId openedStory

    _ ->
      False


placeholderView : Html a
placeholderView =
  div [ class "story-link" ]
    [ text "....." ]


storyView : ItemData -> Bool -> Html Msg
storyView storyData active =
  let
    clz =
      "story-link"
        ++
          if active then
            " opened"

          else
            ""
  in
    div [ class clz, onClick (Open storyData.id) ]
      [ text storyData.title ]
