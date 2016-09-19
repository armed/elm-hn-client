module Model exposing (..)

-- vendor

import Dict exposing (Dict)
import Date exposing (Date)


type alias Model =
    { filter : StoryFilter
    , stories : List Item
    , openedStory : Maybe Item
    , currentTime : Date
    , page : Page
    }


type Page
    = HomePage
    | StoryPage Int


type StoryFilter
    = TopStories
    | NewStories
    | AskStories
    | JobStories
    | BestStories


type Item
    = Lite Int
    | Full ItemData


type alias ItemData =
    { id : Int
    , deleted : Bool
    , kind : ItemType
    , by : Maybe String
    , time : Int
    , text : String
    , parent : Maybe Item
    , kids : Dict Int ( Int, Item )
    , url : String
    , score : Int
    , title : String
    , parts : Dict Int ( Int, Item )
    , descendants : Int
    }


type ItemType
    = Job
    | Story
    | Comment
    | Poll
    | Pollopt
    | Unknown


isFull : Item -> Bool
isFull item =
    not (isLite item)


isLite : Item -> Bool
isLite item =
    case item of
        Lite _ ->
            True

        _ ->
            False


itemId : Item -> Int
itemId a =
    case a of
        Lite id ->
            id

        Full data ->
            data.id


mapFull : Item -> (ItemData -> Item) -> Item
mapFull item func =
    runWithDefault item func item


runWithDefault : a -> (ItemData -> a) -> Item -> a
runWithDefault default func item =
    case item of
        Full data ->
            func data

        _ ->
            default


toMaybe : Item -> Maybe ItemData
toMaybe item =
    case item of
        Full data ->
            Just data

        Lite _ ->
            Nothing
