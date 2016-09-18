module Msg exposing (..)

-- local

import Model exposing (Item)
import Date exposing (Date)


type Msg
    = ItemIdsLoad (List Int)
    | ItemLoad (List Int) Item
    | UnexpectedError String
    | OpenStory Int
    | CloseStory
    | CurrentTime Date
    | EmptyMsg
