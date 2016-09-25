module Update exposing (update, urlUpdate)

-- vendor

import Navigation
import Dict exposing (Dict)
import Maybe exposing (andThen, withDefault)
import Maybe.Extra exposing (mapDefault)


-- local

import Msg exposing (..)
import Model exposing (..)
import Nav
import Ports


-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoryIdsLoaded list ->
            let
                itemDataRequests =
                    List.reverse list
                        |> List.map Ports.getStoryData
            in
                { model | storyIds = list }
                    ! itemDataRequests

        StoryDataLoaded id data ->
            let
                updatedModel =
                    { model
                        | stories = itemDictUpdater id data model.stories
                    }
            in
                updatedModel ! storyCommentRequests updatedModel id

        CommentDataLoaded id data ->
            { model
                | openedStory =
                    Maybe.map
                        (\os -> { os | comments = itemDictUpdater id data os.comments })
                        model.openedStory
            }
                ! List.map Ports.getCommentData data.kids

        -- TODO: handle someway
        UnexpectedError msg ->
            let
                _ =
                    Debug.log "error" msg
            in
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


itemDictUpdater : Int -> ItemData -> (ItemDataDict -> ItemDataDict)
itemDictUpdater itemId itemData =
    Dict.update itemId (\_ -> Just itemData)


storyCommentRequests : Model -> Int -> List (Cmd Msg)
storyCommentRequests model storyId =
    model.openedStory
        `andThen`
            (\{ id } ->
                if id == storyId then
                    Dict.get storyId model.stories
                else
                    Nothing
            )
        `andThen` (.kids >> List.map Ports.getCommentData >> Just)
        |> withDefault []


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
            let
                updatedModel =
                    { model
                        | page = page
                        , openedStory = Just (openedStoryFromId id)
                    }
            in
                updatedModel ! storyCommentRequests updatedModel id
