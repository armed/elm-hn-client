module Subscriptions exposing (..)

-- vendor
import Json.Decode as Json
import Time
import Keyboard
import Date

-- local
import Model exposing (Model)
import Msg exposing (Msg (..))
import Decode
import Ports exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    handleEscKey keyCode =
      if keyCode == 27 then
        CloseStory
      else
        EmptyMsg

    tenSeconds = Time.second * 10
  in
    Sub.batch
      [ itemIds parseItemListJson
      , itemData parseItemDataJson
      , Keyboard.ups handleEscKey
      , Time.every tenSeconds (Date.fromTime >> CurrentTime)
      ]


parseItemListJson : List Int -> Msg
parseItemListJson list =
  ItemIdsLoad list


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
