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
                        HomePage ->
                            Cmd.none

                        _ ->
                            Navigation.newUrl <| Nav.toHash model.page

                itemDataCmds =
                    itemDataRequestCmds [] <| List.reverse list
            in
                { model | stories = List.map Lite list }
                    ! (itemDataCmds ++ [ navCmd ])

        ItemLoad pathIds item ->
            if List.length pathIds > 1 then
                Maybe.mapDefault
                    ( model, Cmd.none )
                    (updateOpenedStory model pathIds item)
                    model.openedStory
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


updateOpenedStory : Model -> List Int -> Item -> Item -> ( Model, Cmd Msg )
updateOpenedStory model pathIds newStory oldStory =
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
        Maybe.mapDefault [] (runWithDefault [] cmdMaker) mbStory


updateStoryList : Model -> Item -> ( Model, Cmd Msg )
updateStoryList model updatedItem =
    let
        mapper oldItem =
            if itemId oldItem == itemId updatedItem then
                updatedItem
            else
                oldItem

        firstTimeInit =
            Maybe.mapDefault True isLite model.openedStory

        mappedStory =
            Maybe.map mapper model.openedStory

        -- when app initialized with story url we should try to load story comments
        ( updatedModel, cmds ) =
            if firstTimeInit then
                ( { model | openedStory = mappedStory }
                , loadCommentsCmds mappedStory
                )
            else
                ( model, [] )
    in
        { updatedModel | stories = List.map mapper model.stories }
            ! cmds


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
