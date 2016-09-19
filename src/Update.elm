module Update exposing (update, urlUpdate)

-- vendor

import Dict
import Navigation
import Maybe.Extra as Maybe


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
            let
                navCmd =
                    case model.page of
                        StoryPage id ->
                            Navigation.newUrl <| Nav.toHash model.page

                        _ ->
                            Cmd.none

                itemDataCmds =
                    itemDataRequestCmds [] <| List.reverse list
            in
                { model | stories = List.map Lite list }
                    ! (itemDataCmds ++ [ navCmd ])

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
            model ! []


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


loadCommentsCmds : Maybe Item -> List (Cmd Msg)
loadCommentsCmds mbStory =
    let
        cmdMaker data =
            itemDataRequestCmds [ data.id ] <| Dict.keys data.kids
    in
        case mbStory of
            Just story ->
                -- loading comments only is story isFull
                runWithDefault story cmdMaker []

            Nothing ->
                []


updateStoryList : Model -> Item -> ( Model, Cmd Msg )
updateStoryList model updatedItem =
    let
        mapper oldItem =
            if itemId oldItem == itemId updatedItem then
                updatedItem
            else
                oldItem

        updatedModel =
            { model
                | stories = List.map mapper model.stories
                , openedStory = Maybe.map mapper model.openedStory
            }

        openedStoryWasntLoaded =
            Maybe.mapDefault True isLite model.openedStory

        -- when app loaded with story url we should try to load story comments
        cmds =
            if openedStoryWasntLoaded then
                loadCommentsCmds updatedModel.openedStory
            else
                []
    in
        updatedModel ! cmds


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
                            ! (loadCommentsCmds mbStory)
                   )
