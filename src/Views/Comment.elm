module Views.Comment exposing (update, comments)

-- vendor

import Html exposing (Html, div, text, label, input)
import Html.Keyed as Keyed
import Html.Attributes exposing (id, class, attribute, for, type')
import Html.Attributes.Extra exposing (innerHtml)
import Dict exposing (Dict)
import Date exposing (Date)
import Maybe.Extra as Maybe


-- local

import Views.TimeLabel exposing (timeLabel)
import Model
    exposing
        ( Item(..)
        , ItemData
        , itemId
        , mapFull
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
        ( updateComment oldComment pathIds newComment
        , runWithDefault [] idsToFetch newComment
        )


updateComment : Item -> List Int -> Item -> Item
updateComment oldComment pathIds newComment =
    let
        updateCommentInDict =
            updateInDict pathIds newComment

        updateData id data =
            Full <|
                { data
                    | kids = Dict.update id updateCommentInDict data.kids
                }
    in
        Maybe.mapDefault
            newComment
            (mapFull oldComment << updateData)
            (List.head pathIds)


updateInDict : List Int -> Item -> Maybe ( Int, Item ) -> Maybe ( Int, Item )
updateInDict pathIds newComment mbOldComment =
    case mbOldComment of
        Just ( index, oldComment ) ->
            Just ( index, updateComment oldComment (List.drop 1 pathIds) newComment )

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
