module Msg exposing (..)

-- local
import Components.StoryLink as StoryLink
import Model exposing (Item)


type Msg
  = ItemIdsLoad (List Int)
  | ItemLoad (List Int) Item
  | UnexpectedError String
  | OpenStory StoryLink.Msg
  | CloseStory
  | CurrentTime Float
  | EmptyMsg
