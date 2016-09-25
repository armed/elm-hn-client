module Views.Story exposing (..)

-- vendor

import Html exposing (Html, div, h4, text)
import Html.Keyed as Keyed
import Html.Attributes exposing (id, class)
import Html.Attributes.Extra exposing (innerHtml)
import Date exposing (Date)
import Maybe.Extra as Maybe
import Dict


-- local

import Model exposing (Model, OpenedStory)
import Views.Comment as Comment


view : Model -> Html a
view model =
    div [ class "story" ] <|
        Maybe.mapDefault [] (storyBody model) model.openedStory


storyBody : Model -> OpenedStory -> List (Html a)
storyBody { stories, currentTime } { id, comments } =
    let
        sb storyData =
            [ div [ class "story-body" ]
                [ div [ class "story-text", innerHtml storyData.text ] []
                , Keyed.node "div" [ class "story-comments" ] <|
                    Comment.view currentTime comments storyData
                ]
            ]
    in
        Maybe.mapDefault [] sb (Dict.get id stories)
