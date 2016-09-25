module Subscriptions exposing (..)

-- vendor

import Json.Decode as Json
import Time
import Keyboard
import Date


-- local

import Model exposing (Model, ItemData)
import Msg exposing (Msg(..))
import Decode
import Ports


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        handleEscKey keyCode =
            if keyCode == 27 then
                CloseStory
            else
                EmptyMsg

        tenSeconds =
            Time.second * 10
    in
        Sub.batch
            [ Ports.storyIds StoryIdsLoaded
            , Ports.storyData (parseItemDataJson StoryDataLoaded)
            , Ports.commentData (parseItemDataJson CommentDataLoaded)
            , Keyboard.ups handleEscKey
            , Time.every tenSeconds (Date.fromTime >> CurrentTime)
            ]


parseItemDataJson : (Int -> ItemData -> Msg) -> ( Int, Json.Value ) -> Msg
parseItemDataJson msgSender ( itemId, json ) =
    let
        resultToMsg result =
            case result of
                Result.Ok item ->
                    msgSender itemId item

                Result.Err msg ->
                    UnexpectedError msg
    in
        Json.decodeValue Decode.itemData json
            |> resultToMsg
