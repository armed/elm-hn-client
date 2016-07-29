module Components.StoryLink exposing (view, Msg (..))

-- vendor
import Html exposing (Html, div, text, a)
import Html.Attributes exposing (id, class, href, target)
import Html.Events exposing (onClick)

-- local
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
  div [ class "story-link" ] []


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
    div [ class clz ]
      [ div [ class "story-title", onClick (Open storyData.id) ]
          [ div [ class "title-text" ] [ text storyData.title ]
          , div [ class "url" ]
              [ a [ href storyData.url, target "_blank" ]
                  [ text storyData.url ]
              ]
          ]
      , div [ class "story-info" ]
          [ div [ class "story-score" ]
              [ text <| toString storyData.score ]
          , div [ class "story-comments" ]
              [ text <| toString storyData.descendants ]
          ]
      ]
