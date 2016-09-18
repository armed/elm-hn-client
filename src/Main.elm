-- vendor


module Main exposing (..)

import Html exposing (Html, div, text, h3)
import Html.Attributes exposing (class)
import Html.App as App
import String
import Time
import Task
import Date


-- local

import Model exposing (Model, StoryFilter(..), isFull)
import Views.Story as Story
import Views.Header as Header
import Views.StoryLink as StoryLink
import Subscriptions exposing (subscriptions)
import Msg exposing (Msg(..))
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


init : ( Model, Cmd Msg )
init =
    let
        defaultFilter =
            TopStories

        defaultDate =
            Date.fromTime 0
    in
        Model defaultFilter [] Nothing defaultDate
            ! [ Task.perform UnexpectedError (Date.fromTime >> CurrentTime) Time.now
              , Ports.getItemIds <| String.toLower <| toString defaultFilter
              ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ Header.view model.openedStory
        , Story.view model.openedStory model.currentTime
        , storyLinks model
        ]


storyLinks : Model -> Html Msg
storyLinks { stories, openedStory, currentTime } =
    let
        storyLink story =
            StoryLink.view story currentTime openedStory

        storiesWithData =
            List.filter isFull stories

        links =
            div [ class "links" ] <|
                List.map storyLink storiesWithData
    in
        div [ class "story-list" ]
            [ links ]
