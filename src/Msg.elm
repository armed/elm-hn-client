module Msg exposing (..)

-- local

import Model exposing (ItemData)
import Date exposing (Date)


type Msg
    = StoryIdsLoaded (List Int)
    | StoryDataLoaded Int ItemData
    | CommentDataLoaded Int ItemData
    | UnexpectedError String
    | OpenStory Int
    | CloseStory
    | CurrentTime Date
    | EmptyMsg
