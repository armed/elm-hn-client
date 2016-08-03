module Components.StoryLink exposing (view, Msg(..))

-- vendor

import Html exposing (Html, div, text, a)
import Html.Attributes exposing (id, class, href, target)
import Html.Events exposing (onClick)
import Date exposing (Date)


-- local

import Components.FaIcon exposing (faIcon)
import Components.TimeLabel exposing (timeLabel)
import Model exposing (Model, Item(..), ItemData, itemId)


type Msg
    = Open Int


view : Item -> Date -> Maybe Item -> Html Msg
view story currentDate mbOpenedStory =
    case story of
        Lite _ ->
            placeholderView

        Full storyData ->
            storyView storyData currentDate <| isActive story mbOpenedStory


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


storyView : ItemData -> Date -> Bool -> Html Msg
storyView storyData currentDate active =
    let
        clz =
            "story-link"
                ++ if active then
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
