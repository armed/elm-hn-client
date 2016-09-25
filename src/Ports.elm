port module Ports exposing (..)

-- vendor

import Json.Decode as Json


port storyIds : (List Int -> msg) -> Sub msg


port storyData : (( Int, Json.Value ) -> msg) -> Sub msg


port commentData : (( Int, Json.Value ) -> msg) -> Sub msg


port getStoryIds : String -> Cmd msg


port getStoryData : Int -> Cmd msg


port getCommentData : Int -> Cmd msg
