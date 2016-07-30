-- vendor
import Html exposing (Html, div, text, h3)
import Html.Attributes exposing (class)
import Html.App as App
import Json.Decode as Json
import String
import Dict
import Time
import Task
import Keyboard

-- local
import Decode
import Model exposing (Model, StoryFilter (..), isFull)
import Components.Story as Story
import Components.Header as Header
import Components.StoryLink as StoryLink
import Subscriptions exposing (subscriptions)
import Msg exposing (Msg (..))
import Update exposing (update)
import Ports


-- APP


main : Program Never
main =
  App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- INIT


init : (Model, Cmd Msg)
init =
  let
    defaultFilter = TopStories
  in
    Model defaultFilter [] Nothing 0
      ! [ Task.perform UnexpectedError CurrentTime Time.now
        , Ports.getItemIds <| String.toLower <| toString defaultFilter
        ]


-- VIEW


view : Model -> Html Msg
view model =
  div [ class "content" ]
    [ Header.view model.openedStory
    , Story.view model.openedStory
    , storyLinks model
    ]


storyLinks : Model -> Html Msg
storyLinks { stories, openedStory } =
  let
    storyLink story =
      App.map OpenStory <| StoryLink.view story openedStory

    storiesWithData =
      List.filter isFull stories

    links =
      div [ class "links" ]
        <| List.map storyLink storiesWithData
  in
    div [ class "story-list" ]
      [ links ]
