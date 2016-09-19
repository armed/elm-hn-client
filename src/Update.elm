module Update exposing (update, urlUpdate)

-- vendor

import Dict
import Navigation


-- local

import Msg exposing (..)
import Model exposing (..)
import Ports exposing (..)
import Nav
import Views.Comment as Comment


-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemIdsLoad list ->
            { model | stories = List.map Lite list }
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
            ( model, Cmd.none )

        OpenStory id ->
            ( model, Navigation.newUrl <| Nav.toHash (StoryPage id) )

        CloseStory ->
            ( model, Navigation.newUrl <| Nav.toHash HomePage )

        CurrentTime time ->
            { model | currentTime = time }
                ! []

        EmptyMsg ->
            ( model, Cmd.none )


updateOpenedStory : Model -> Item -> List Int -> Item -> ( Model, Cmd Msg )
updateOpenedStory model oldStory pathIds newStory =
    let
        rootId =
            Maybe.withDefault -1 (List.head pathIds)
    in
        if itemId oldStory == rootId then
            let
                ( updatedStory, idsToLoad ) =
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


updateStoryList : Model -> Item -> ( Model, Cmd Msg )
updateStoryList model item =
    { model | stories = updateStory item model.stories }
        ! []


updateStory : Item -> List Item -> List Item
updateStory updatedItem listOfItems =
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
    pathIds ++ [ i ]



-- urlUpdate


urlUpdate : Result String Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err _ ->
            ( model, Navigation.modifyUrl (Nav.toHash model.page) )

        Ok HomePage ->
            { model
                | page = HomePage
                , openedStory = Nothing
            }
                ! []

        Ok ((StoryPage id) as page) ->
            List.filter (\s -> itemId s == id) model.stories
                |> List.head
                |> (\mbStory ->
                        { model
                            | openedStory = mbStory
                            , page = page
                        }
                            ! (loadComments mbStory)
                   )
