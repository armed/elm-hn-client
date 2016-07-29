module Subscriptions exposing (..)

-- vendor
import Json.Decode as Json
import Time
import Keyboard

-- local
import Model exposing (Model)
import Msg exposing (Msg (..))
import Decode
import Ports exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    handleEscKey keyCode =
      if (Debug.log "keyCode" keyCode) == 27 then
        CloseStory
      else
        EmptyMsg

    tenSeconds = Time.second * 10
  in
    Sub.batch
      [ itemIds parseItemListJson
      , itemData parseItemDataJson
      , Keyboard.ups handleEscKey
      , Time.every tenSeconds CurrentTime
      ]


parseItemListJson : List Int -> Msg
parseItemListJson list =
  ItemIdsLoad <| List.reverse list


parseItemDataJson : (List Int, Json.Value) -> Msg
parseItemDataJson (pathIds, json) =
  let
    resultToMsg pathIds result =
      case result of
        Result.Ok item ->
          ItemLoad pathIds item

        Result.Err msg ->
          UnexpectedError msg
  in
    Json.decodeValue Decode.item json
      |> resultToMsg pathIds
