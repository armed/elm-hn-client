module Views.Story exposing (..)

-- vendor

import Html exposing (Html, div, h4, text)
import Html.Keyed as Keyed
import Html.Attributes exposing (id, class)
import Html.Attributes.Extra exposing (innerHtml)
import Date exposing (Date)
import Maybe.Extra as Maybe


-- local

import Model exposing (Item(..), ItemData, runWithDefault)
import Views.Comment as Comment exposing (comments)


view : Maybe Item -> Date -> Html a
view mbStory currentDate =
    let
        defaultFn =
            runWithDefault emptyStory (fullStory currentDate)
    in
        div [ class "story" ] <|
            Maybe.mapDefault emptyStory defaultFn mbStory


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
