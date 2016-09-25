module Views.Header exposing (..)

-- vendor

import Html exposing (Html, div, text, a, span, i)
import Html.Attributes exposing (class, href, title, target)
import Html.Events exposing (onClick)
import Maybe.Extra exposing (mapDefault)
import Maybe exposing (andThen, withDefault)
import Dict


-- local

import Views.FaIcon exposing (faIcon)
import Model exposing (Model, OpenedStory)
import Msg exposing (Msg(CloseStory))


view : Model -> Html Msg
view { openedStory, stories } =
    let
        storyTitle : OpenedStory -> Maybe String
        storyTitle { id } =
            (Dict.get id stories)
                `andThen` (.title >> Just)

        ( isStoryOpened, titleText ) =
            openedStory
                `andThen` storyTitle
                |> mapDefault ( False, "" ) ((,) True)

        clz =
            if isStoryOpened then
                " story-opened"
            else
                ""
    in
        div [ class <| "header" ++ clz ]
            [ githubLink
            , span [ class "logo" ] []
            , a [ href "#", class "back-btn", onClick CloseStory ]
                [ faIcon "arrow-left" ]
            , div [ class "title" ]
                [ text titleText ]
            ]


githubLink : Html a
githubLink =
    a
        [ class "github-link"
        , href "https://github.com/armed/elm-hn-client"
        , title "Open Github repo"
        , target "_blank"
        ]
        [ faIcon "github" ]
