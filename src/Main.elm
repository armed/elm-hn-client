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
import Components.StoryLink as StoryLink
import Components.ForkMeOnGithub exposing (forkMeOnGithub)
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
    [ Story.view model.openedStory
    , storyList model
    , forkMeOnGithub
    ]


storyList : Model -> Html Msg
storyList { stories, openedStory } =
  let
    storyLink story =
      StoryLink.view story openedStory

    stories' =
      List.filter isFull stories
  in
    div [ class "story-list" ]
      (div [ class "header" ] []
        :: List.map (storyLink >> App.map OpenStory) stories'
      )
