module Update exposing (update)

-- vendor
import Dict

-- local
import Msg exposing (..)
import Model exposing (..)
import Ports exposing (..)
import Components.StoryLink as StoryLink
import Components.Comment as Comment


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ItemIdsLoad list ->
      { model | stories = List.map Lite list  }
        ! (itemDataRequestCmds [] <| List.reverse list)

    ItemLoad pathIds item ->
      if List.length pathIds > 1 then
        case model.openedStory of
          Just story ->
            updateOpenedStory model story pathIds item

          Nothing ->
            model ! []
      else
        updateStoryList model item

    UnexpectedError msg ->
      -- will handle someday
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

    CurrentTime time ->
      { model | currentTime = time }
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
          ! itemDataRequestCmds pathIds idsToLoad
    else
      model ! []


loadComments : Maybe Item -> List (Cmd Msg)
loadComments mbStory =
  let
    cmdMaker data =
      itemDataRequestCmds [ data.id ] <| Dict.keys data.kids
  in
    case mbStory of
      Just story ->
        runWithDefault story cmdMaker []

      Nothing ->
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


itemDataRequestCmds : List Int -> List Int -> List (Cmd Msg)
itemDataRequestCmds pathIds list =
  List.map (appendTo pathIds >> getItemData) list


appendTo : List Int -> Int -> List Int
appendTo pathIds i =
  pathIds ++ [i]
