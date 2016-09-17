module Components.Story exposing (..)

-- vendor

import Html exposing (Html, div, h4, text)
import Html.Keyed as Keyed
import Html.Attributes exposing (id, class)
import Html.Attributes.Extra exposing (innerHtml)
import Date exposing (Date)


-- local

import Model exposing (Item(..), ItemData, runWithDefault)
import Components.Comment as Comment exposing (comments)


view : Maybe Item -> Date -> Html a
view mbStory currentDate =
    let
        renderStory mbStory =
            case mbStory of
                Just storyItem ->
                    runWithDefault storyItem (fullStory currentDate) emptyStory

                Nothing ->
                    emptyStory
    in
        div [ class "story" ] <| renderStory mbStory


emptyStory : List (Html a)
emptyStory =
    []


fullStory : Date -> ItemData -> List (Html a)
fullStory currentDate story =
    [ div [ class "story-body" ]
        [ div [ class "story-text", innerHtml story.text ] []
        , Keyed.node "div" [ class "story-comments" ] <|
            comments currentDate story.kids
        ]
    ]
