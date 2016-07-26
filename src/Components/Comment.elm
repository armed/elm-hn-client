module Components.Comment exposing (..)

import Html exposing (Html, div, text, label, input)
import Html.Attributes exposing (id, class, attribute, for, type')
import Html.Attributes.Extra exposing (innerHtml)
import Dict exposing (Dict)
import Model exposing (Item (..), ItemData, itemId, ifFullThen, runWithDefault, isLite)


type alias Model = Item


type alias Comment = Item


type alias IdsPath = List Int


update : Model -> Comment -> IdsPath -> (Model, List Int)
update model cmt idsPath =
  let
    updateKids id kids idsPath' =
      Dict.update id (updateKid idsPath') kids

    updateKid idsPath' mbKid =
      case mbKid of
        Just (index, kid) ->
          Just <| (index, update' kid cmt <| List.drop 1 idsPath')

        _ ->
          Nothing

    update' model' cmt' idsPath' =
      let
        _ = Debug.log "model and idsPath" (model', idsPath')
      in
        case List.head idsPath' of
          Just id ->
            model' `ifFullThen` (\data ->
              { data
              | kids = updateKids id data.kids idsPath'
              } |> Full
              )

          _ ->
            cmt'

    idsToLoad data =
      Dict.values data.kids
        |> List.map snd
        |> List.filter isLite
        |> List.map itemId
  in
    ( update' model cmt idsPath
    , runWithDefault cmt idsToLoad []
    )


comment : Model -> Html a
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
              [ text <| mwd data.by ]
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


comments : Dict Int (Int, Model) -> List (Html a)
comments commentsData =
  Dict.values commentsData
    |> List.sortBy fst
    |> List.map snd
    |> List.filter deletedFilter
    |> List.map comment


deletedFilter : Item -> Bool
deletedFilter cmt =
  case cmt of
    Full data ->
      not data.deleted

    _ ->
      True
