port module Main exposing (..)

import Html exposing (Html, div, text, h3)
import Html.Attributes exposing (class)
import Html.App as App
import Model exposing
  ( Model
  , Item (..)
  , ItemData
  , itemId
  , StoryFilter (..)
  , ifFullThen
  , runWithDefault
  , isFull
  )
import Keyboard
import Components.Comment as Comment
import Components.StoryLink as StoryLink
import Components.Story as Story
import Json.Decode as Json
import Decode
import String
import Dict


-- APP


main : Program Never
main =
  App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL


init : (Model, Cmd Msg)
init =
  let
    defaultFilter = TopStories
  in
    Model defaultFilter [] Nothing
      ! [ getItemIds <| String.toLower <| toString defaultFilter ]


-- UPDATE


type Msg
  = ItemIdsLoad (List Int)
  | ItemLoad (List Int) Item
  | ItemLoadError String
  | OpenStory StoryLink.Msg
  | CloseStory
  | EmptyMsg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ItemIdsLoad list ->
      { model | stories = List.map Lite list  }
        ! (itemDataCmds [] list)

    ItemLoad pathIds item ->
      if List.length pathIds > 1 then
        case model.openedStory of
          Just story ->
            updateOpenedStory model story pathIds item

          Nothing ->
            model ! []
      else
        updateStoryList model item

    ItemLoadError msg ->
      let
        _ = Debug.log "err msg" msg
      in
        (model, Cmd.none)

    OpenStory (StoryLink.Open id) ->
      List.filter (\s -> itemId s == id) model.stories
        |> List.head
        |> (\mbStory ->
          { model | openedStory = mbStory }
            ! (loadComments mbStory)
        )

    CloseStory ->
      { model | openedStory = Nothing }
        ! []

    EmptyMsg ->
      (model, Cmd.none)


updateOpenedStory : Model -> Item -> List Int -> Item -> (Model, Cmd Msg)
updateOpenedStory model oldStory pathIds newStory =
  let
    rootId = Maybe.withDefault -1 (List.head pathIds)
  in
    if itemId oldStory == rootId then
      let
        (updatedStory, idsToLoad) =
          Comment.update oldStory newStory <| List.drop 1 pathIds
      in
        { model | openedStory = Just updatedStory }
          ! itemDataCmds pathIds idsToLoad
    else
      model ! []


loadComments : Maybe Item -> List (Cmd msg)
loadComments mbStory =
  let
    cmdMaker data =
      itemDataCmds [ data.id ] <| Dict.keys data.kids
  in
    case mbStory of
      Just story ->
        runWithDefault story cmdMaker []

      Nothing ->
        []


commentsCmds : List Int -> Item -> List (Cmd Msg)
commentsCmds pathIds item =
  case Debug.log "item" item of
    Full itemData ->
      itemDataCmds pathIds <| Dict.keys itemData.kids

    _ ->
      []


updateStoryList : Model -> Item -> (Model, Cmd Msg)
updateStoryList model item =
  { model | stories = updateStoryInList item model.stories }
    ! []


updateStoryInList : Item -> List Item -> List Item
updateStoryInList updatedItem listOfItems =
  let
    mapper oldItem =
      if itemId oldItem == itemId updatedItem then
        updatedItem
      else
        oldItem
  in
    List.map mapper listOfItems


itemDataCmds : List Int -> List Int -> List (Cmd msg)
itemDataCmds pathIds list =
  List.map (appendTo pathIds >> getItemData) list


appendTo : List Int -> Int -> List Int
appendTo pathIds i =
  pathIds ++ [i]


-- SUBSCRIPTIONS


port itemIds : (List Int -> msg) -> Sub msg


port itemData : ((List Int, Json.Value) -> msg) -> Sub msg


port getItemIds : String -> Cmd msg


port getItemData : List Int -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    parseItemListJson list =
      ItemIdsLoad <| List.reverse list

    parseItemDataJson (pathIds, json) =
      Json.decodeValue Decode.item json
        |> resultToMsg pathIds

    resultToMsg pathIds result =
      case result of
        Result.Ok item ->
          ItemLoad pathIds item

        Result.Err msg ->
          ItemLoadError msg

    handleEscKey keyCode =
      if (Debug.log "keyCode" keyCode) == 27 then
        CloseStory
      else
        EmptyMsg
  in
    Sub.batch
      [ itemIds parseItemListJson
      , itemData parseItemDataJson
      , Keyboard.ups handleEscKey
      ]


-- VIEW


view : Model -> Html Msg
view model =
  div [ class "content" ]
    [ Story.view model.openedStory
    , storyList model
    ]


storyList : Model -> Html Msg
storyList { stories, openedStory } =
  let
    storyLink story =
      StoryLink.view story openedStory

    stories' =
      List.filter isFull stories
  in
    div [ class "story-list" ]
      (div [ class "header" ] []
        :: List.map (storyLink >> App.map OpenStory) stories'
      )
