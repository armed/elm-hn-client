module Views.Header exposing (..)

-- vendor

import Html exposing (Html, div, text, a, span, i)
import Html.Attributes exposing (class, href, title, target)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe


-- local

import Views.FaIcon exposing (faIcon)
import Msg exposing (Msg(CloseStory))
import Model exposing (Item, runWithDefault)


view : Maybe Item -> Html Msg
view mbStory =
    let
        defaultFn =
            (,) True << runWithDefault "" .title

        ( isStoryOpened, titleText ) =
            Maybe.mapDefault
                ( False, "" )
                defaultFn
                mbStory

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
