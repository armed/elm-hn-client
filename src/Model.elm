module Model
    exposing
        ( Model
        , Page(..)
        , StoryFilter(..)
        , ItemData
        , ItemDataDict
        , ItemType(..)
        , OpenedStory
        , openedStoryFromId
        , openedStoryFromPage
        )

-- vendor

import Dict exposing (Dict)
import Date exposing (Date)


type alias ItemDataDict =
    Dict Int ItemData


type alias Model =
    { filter : StoryFilter
    , storyIds : List Int
    , stories : ItemDataDict
    , openedStory : Maybe OpenedStory
    , currentTime : Date
    , page : Page
    }


type alias OpenedStory =
    { id : Int
    , comments : ItemDataDict
    }


openedStoryFromId : Int -> OpenedStory
openedStoryFromId id =
    OpenedStory id Dict.empty


openedStoryFromPage : Page -> Maybe OpenedStory
openedStoryFromPage page =
    case page of
        StoryPage id ->
            Just <| openedStoryFromId id

        _ ->
            Nothing


type Page
    = HomePage
    | StoryPage Int


type StoryFilter
    = TopStories
    | NewStories
    | AskStories
    | JobStories
    | BestStories


type alias ItemData =
    { id : Int
    , deleted : Bool
    , kind : ItemType
    , by : Maybe String
    , time : Int
    , text : String
    , kids : List Int
    , url : String
    , score : Int
    , title : String
    , descendants : Int
    }


type ItemType
    = Job
    | Story
    | Comment
    | Poll
    | Pollopt
    | Unknown
