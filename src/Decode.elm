module Decode exposing (storyIds, itemData)

-- vendor

import Json.Decode.Extra exposing ((|:))
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


storyIds : Decoder (List Int)
storyIds =
    list int


itemData : Decoder ItemData
itemData =
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
            |: default "kids" (list int) []
            |: defaultStr "url"
            |: default "score" int 0
            |: defaultStr "title"
            |: default "descendants" int 0


default : String -> Decoder a -> a -> Decoder a
default field decoder defVal =
    oneOf [ field := decoder, succeed defVal ]


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
