module Decode exposing (..)

-- vendor

import Json.Decode.Extra exposing ((|:), lazy)
import Json.Decode as Json
    exposing
        ( Decoder
        , string
        , bool
        , int
        , list
        , succeed
        , oneOf
        , map
        , andThen
        , (:=)
        , maybe
        , decodeString
        , customDecoder
        , value
        , decodeValue
        )
import Dict exposing (Dict)


-- local

import Model exposing (..)


items : Decoder (List Item)
items =
    list item


item : Decoder Item
item =
    oneOf
        [ map Lite int
        , map Full <| lazy (\_ -> full)
        ]


full : Decoder ItemData
full =
    let
        defaultStr field =
            default field string ""
    in
        map ItemData ("id" := int)
            |: default "deleted" bool False
            |: ("type" := itemType)
            |: ("by" := string |> maybe)
            |: ("time" := int)
            |: defaultStr "text"
            |: ("parent" := item |> maybe)
            |: default "kids" (lazy <| \_ -> itemDict) Dict.empty
            |: defaultStr "url"
            |: default "score" int 0
            |: defaultStr "title"
            |: default "parts" (lazy <| \_ -> itemDict) Dict.empty
            |: default "descendants" int 0


default : String -> Decoder a -> a -> Decoder a
default field decoder defVal =
    oneOf [ field := decoder, succeed defVal ]


itemListToDict : List Item -> Result a (Dict Int ( Int, Item ))
itemListToDict items =
    let
        addItemToDict indexedItem dict =
            Dict.insert (itemId <| snd indexedItem) indexedItem dict
    in
        items
            |> List.indexedMap (,)
            |> List.foldl addItemToDict Dict.empty
            |> Result.Ok


itemDict : Decoder (Dict Int ( Int, Item ))
itemDict =
    customDecoder (list item) itemListToDict


itemType : Decoder ItemType
itemType =
    let
        stringToItemKind typeName =
            case typeName of
                "job" ->
                    Job

                "story" ->
                    Story

                "comment" ->
                    Comment

                "poll" ->
                    Poll

                "pollopt" ->
                    Pollopt

                _ ->
                    Unknown
    in
        string `andThen` (stringToItemKind >> succeed)
