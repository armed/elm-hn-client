module Components.Comment exposing (update, comments)

-- vendor

import Html exposing (Html, div, text, label, input)
import Html.Keyed as Keyed
import Html.Attributes exposing (id, class, attribute, for, type')
import Html.Attributes.Extra exposing (innerHtml)
import Dict exposing (Dict)
import Date exposing (Date)


-- local

import Components.TimeLabel exposing (timeLabel)
import Model
    exposing
        ( Item(..)
        , ItemData
        , itemId
        , ifFullThen
        , runWithDefault
        , isLite
        , isFull
        , toMaybe
        )


type alias Comment =
    Item


type alias PathIds =
    List Int


type alias IdsToFetch =
    List Int


update : Comment -> Comment -> PathIds -> ( Comment, IdsToFetch )
update oldComment newComment pathIds =
    let
        idsToFetch data =
            Dict.values data.kids
                |> List.map snd
                |> List.filter isLite
                |> List.map itemId
    in
        ( updateComment oldComment newComment pathIds
        , runWithDefault newComment idsToFetch []
        )


updateComment : Item -> Item -> List Int -> Item
updateComment oldComment newComment pathIds =
    let
        updateCommentInDict =
            updateInDict pathIds newComment

        updateData id data =
            Full <|
                { data
                    | kids = Dict.update id updateCommentInDict data.kids
                }
    in
        case List.head pathIds of
            Just id ->
                oldComment `ifFullThen` (updateData id)

            _ ->
                newComment


updateInDict : List Int -> Item -> Maybe ( Int, Item ) -> Maybe ( Int, Item )
updateInDict pathIds newComment mbOldComment =
    case mbOldComment of
        Just ( index, oldComment ) ->
            Just <| ( index, updateComment oldComment newComment <| List.drop 1 pathIds )

        _ ->
            Nothing


comment : Date -> ItemData -> ( String, Html a )
comment currentTime data =
    let
        cbId =
            "cb" ++ toString data.id
    in
        ( cbId
        , div
            [ class "comment" ]
            [ input [ id cbId, type' "checkbox" ] []
            , label [ class "comment-header", for cbId ]
                [ div [ class "arrow" ] []
                , div [ class "nickname" ]
                    [ text <| Maybe.withDefault "" data.by ]
                , div [ class "time" ]
                    [ timeLabel currentTime data.time ]
                ]
            , div [ class "comment-body", innerHtml data.text ]
                []
            , Keyed.node "div" [ class "comment-kids" ] <|
                comments currentTime data.kids
            ]
        )


comments : Date -> Dict Int ( Int, Comment ) -> List ( String, Html a )
comments currentTime commentsData =
    let
        mbMapper item =
            (item |> snd >> toMaybe) `Maybe.andThen` notDeleted
    in
        Dict.values commentsData
            |> List.sortBy fst
            |> List.filterMap mbMapper
            |> List.map (comment currentTime)


notDeleted : ItemData -> Maybe ItemData
notDeleted data =
    if not data.deleted then
        Just data
    else
        Nothing
