module Views.Comment exposing (view)

-- vendor

import Html exposing (Html, div, text, label, input)
import Html.Keyed as Keyed
import Html.Attributes exposing (id, class, attribute, for, type')
import Html.Attributes.Extra exposing (innerHtml)
import Date exposing (Date)
import Maybe.Extra as Maybe
import Dict exposing (Dict)


-- local

import Views.TimeLabel exposing (timeLabel)
import Model exposing (ItemData)


view : Date -> Dict Int ItemData -> ItemData -> List ( String, Html a )
view currentTime commentsDict commentData =
    List.map (comment currentTime commentsDict) commentData.kids


comment : Date -> Dict Int ItemData -> Int -> ( String, Html a )
comment currentTime commentsDict commentId =
    let
        cbId =
            "cb" ++ toString commentId

        toTuple cbody =
            ( cbId, div [ class "comment" ] cbody )
    in
        Dict.get commentId commentsDict
            |> Maybe.mapDefault
                []
                (commentBody currentTime commentsDict cbId)
            |> toTuple


commentBody : Date -> Dict Int ItemData -> String -> ItemData -> List (Html a)
commentBody currentTime commentsDict cbId commentData =
    if commentData.deleted then
        []
    else
        [ input [ id cbId, type' "checkbox" ] []
        , label [ class "comment-header", for cbId ]
            [ div [ class "arrow" ] []
            , div [ class "nickname" ]
                [ text <| Maybe.withDefault "" commentData.by ]
            , div [ class "time" ]
                [ timeLabel currentTime commentData.time ]
            ]
        , div [ class "comment-body", innerHtml commentData.text ] []
        , Keyed.node "div" [ class "comment-kids" ] <|
            view currentTime commentsDict commentData
        ]
