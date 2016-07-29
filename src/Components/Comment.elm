module Components.Comment exposing (..)

import Html exposing (Html, div, text, label, input)
import Html.Attributes exposing (id, class, attribute, for, type')
import Html.Attributes.Extra exposing (innerHtml)
import Dict exposing (Dict)
import Model exposing (Item (..), ItemData, itemId, ifFullThen, runWithDefault, isLite)


type alias Comment = Item


type alias PathIds = List Int


type alias IdsToFetch = List Int


update : Comment -> Comment -> PathIds -> (Comment, IdsToFetch)
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
  case List.head pathIds of
    Just id ->
      oldComment `ifFullThen` (\data ->
        Full <|
          { data
          | kids = Dict.update id (updateInDict pathIds newComment) data.kids
          }
      )

    _ ->
      newComment


updateInDict : List Int -> Item -> Maybe (Int, Item) -> Maybe (Int, Item)
updateInDict pathIds newComment mbOldComment =
  case mbOldComment of
    Just (index, oldComment) ->
      Just <| (index, updateComment oldComment newComment <| List.drop 1 pathIds)

    _ ->
      Nothing


comment : Comment -> Html a
comment cmt =
  let
    cbId data =
      "cb" ++ toString data.id

    mwd =
      Maybe.withDefault ""

    comment' =
      case cmt of
        Full data ->
          [ input [ id <| cbId data, type' "checkbox" ] []
          , label [ class "comment-header", for <| cbId data ]
              [ div [ class "arrow" ] []
              , div [ class "nickname"]
                  [ text <| mwd data.by ]
              ]
          , div [ class "comment-body", innerHtml data.text ]
              []
          , div [ class "comment-kids" ]
              <| comments data.kids
          ]

        Lite _ ->
          []
  in
    div [ class "comment" ]
      comment'


comments : Dict Int (Int, Comment) -> List (Html a)
comments commentsData =
  Dict.values commentsData
    |> List.sortBy fst
    |> List.map snd
    |> List.filter dropDeleted
    |> List.map comment


dropDeleted : Item -> Bool
dropDeleted cmt =
  case cmt of
    Full data ->
      not data.deleted

    _ ->
      True
