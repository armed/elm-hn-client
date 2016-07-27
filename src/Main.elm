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
  )
import Components.Comment as Comment
import Components.StoryLink as StoryLink
import Components.Story as Story
import Json.Decode as Json
import Decode
import String
import Dict
import Maybe exposing (andThen)


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
  = ItemIdsLoad (List Item)
  | ItemLoad (List Int) Item
  | ItemLoadError String
  | OpenStory StoryLink.Msg
  | CloseStory Story.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ItemIdsLoad list ->
      { model | stories = list }
        ! reverseMap (itemId >> (\i -> [i]) >> getItemData) list

    ItemLoad pathIds item ->
      case model.openedStory of
        Nothing ->
          updateStoryList model item

        Just story ->
          let
            (updatedStory, idsToLoad) =
              Comment.update story item <| List.drop 1 pathIds
          in
            { model
            | openedStory = Just updatedStory }
              ! List.map ((\i -> pathIds ++ [i]) >> getItemData) idsToLoad

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

    CloseStory _ ->
      { model | openedStory = Nothing }
        ! []


reverseMap : (b -> Cmd a) -> List b -> List (Cmd a)
reverseMap func list =
  List.reverse list
    |> List.map func


loadComments : Maybe Item -> List (Cmd msg)
loadComments mbStory =
  case mbStory of
    Just story ->
      runWithDefault story (\data ->
        Dict.keys data.kids
          |> List.map ((\i ->  [ data.id, i ]) >> getItemData)
      ) []

    Nothing ->
      []


commentsCmds : List Int -> Item -> List (Cmd Msg)
commentsCmds pathIds item =
  case Debug.log "item" item of
    Full itemData ->
      Dict.keys itemData.kids
        |> List.map ((\i -> pathIds ++ [i]) >> getItemData)

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


-- SUBSCRIPTIONS


port itemIds : (List Int -> msg) -> Sub msg


port itemData : ((List Int, Json.Value) -> msg) -> Sub msg


port getItemIds : String -> Cmd msg


port getItemData : List Int -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    parseItemListJson list =
      ItemIdsLoad <| List.map Lite list

    parseItemDataJson (pathIds, json) =
      Json.decodeValue Decode.item json
        |> resultToMsg pathIds

    resultToMsg pathIds result =
      case result of
        Result.Ok item ->
          ItemLoad pathIds item

        Result.Err msg ->
          ItemLoadError msg
  in
    Sub.batch
      [ itemIds parseItemListJson
      , itemData parseItemDataJson
      ]


-- VIEW


view : Model -> Html Msg
view model =
  div [ class "content" ]
    [ storyList model
    , Story.view model.openedStory |> App.map CloseStory
    ]


storyList : Model -> Html Msg
storyList { stories, openedStory } =
  let
    storyLink story =
      StoryLink.view story openedStory
  in
    div [ class "story-list" ]
      (div [ class "header" ] []
        :: List.map (storyLink >> App.map OpenStory) stories
      )
