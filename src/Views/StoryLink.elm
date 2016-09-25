module Views.StoryLink exposing (view)

-- vendor

import Html exposing (Html, div, text, a)
import Html.Attributes exposing (id, class, href, target)
import Html.Events exposing (onClick)
import Date exposing (Date)
import Maybe.Extra as Maybe
import Dict


-- local

import Views.FaIcon exposing (faIcon)
import Views.TimeLabel exposing (timeLabel)
import Model exposing (Model, ItemData, OpenedStory)
import Msg exposing (Msg(OpenStory))


view : Model -> Int -> Html Msg
view { currentTime, openedStory, stories } storyId =
    let
        mbOpenedStory =
            Dict.get storyId stories
    in
        Maybe.mapDefault
            (placeholderView storyId)
            (storyView currentTime openedStory)
            mbOpenedStory


placeholderView : Int -> Html a
placeholderView storyId =
    div [ class "story-link" ] [ text "Loading..." ]


storyView : Date -> Maybe OpenedStory -> ItemData -> Html Msg
storyView currentDate mbOpenedStory storyData =
    let
        active =
            Maybe.mapDefault False ((==) storyData.id << .id) mbOpenedStory

        clz =
            "story-link"
                ++ if active then
                    " opened"
                   else
                    ""
    in
        div [ class clz ]
            [ div [ class "story-title", onClick (OpenStory storyData.id) ]
                [ div [ class "title-text" ] [ text storyData.title ]
                , div [ class "url" ]
                    [ a [ href storyData.url, target "_blank" ]
                        [ text storyData.url ]
                    ]
                ]
            , div [ class "story-info" ]
                [ div [ class "story-score" ]
                    [ faIcon "bar-chart"
                    , text <| toString storyData.score
                    ]
                , div [ class "story-comments" ]
                    [ faIcon "comments-o"
                    , text <| toString storyData.descendants
                    ]
                , div [ class "story-time" ]
                    [ timeLabel currentDate storyData.time
                    ]
                ]
            ]
