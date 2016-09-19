-- vendor


module Main exposing (..)

import Html exposing (Html, div, text, h3)
import Html.Attributes exposing (class)
import Navigation
import String
import Time
import Task
import Date
import Result


-- local

import Model exposing (Model, StoryFilter(..), isFull, Page(..))
import Views.Story as Story
import Views.Header as Header
import Views.StoryLink as StoryLink
import Subscriptions exposing (subscriptions)
import Msg exposing (Msg(..))
import Update exposing (update, urlUpdate)
import Nav
import Ports


-- APP
-- main : Program Never


main : Program Never
main =
    Navigation.program Nav.parser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- INIT


init : Result String Page -> ( Model, Cmd Msg )
init result =
    let
        defaultFilter =
            TopStories

        defaultDate =
            Date.fromTime 0

        initPage =
            Result.withDefault HomePage result
    in
        Model defaultFilter [] Nothing defaultDate initPage
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
