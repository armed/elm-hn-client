port module Ports exposing (..)

-- vendor
import Json.Decode as Json


port itemIds : (List Int -> msg) -> Sub msg


port itemData : ((List Int, Json.Value) -> msg) -> Sub msg


port getItemIds : String -> Cmd msg


port getItemData : List Int -> Cmd msg
