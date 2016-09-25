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

import Dict
import Model exposing (Model, StoryFilter(..), Page(..), openedStoryFromPage)
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
        defaultDate =
            Date.fromTime 0

        initPage =
            Result.withDefault HomePage result

        model =
            { filter = TopStories
            , storyIds = []
            , stories = Dict.empty
            , openedStory = openedStoryFromPage initPage
            , currentTime = defaultDate
            , page = initPage
            }
    in
        model
            ! [ Task.perform UnexpectedError (Date.fromTime >> CurrentTime) Time.now
              , Ports.getStoryIds <| String.toLower <| toString TopStories
              ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ Header.view model
        , Story.view model
        , storyLinks model
        ]


storyLinks : Model -> Html Msg
storyLinks model =
    div [ class "story-list" ]
        [ div [ class "links" ] <|
            List.map (StoryLink.view model) model.storyIds
        ]
