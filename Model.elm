port module Model exposing (draggedTask, dropDraggedTaskInto, dropAllTasks, dropTaskIn, moveTaskToGroup, parentGroup, addNewGroup, addNewTask, removeTask, removeGroup, updateTask, updateGroup, loadModel, saveModel, serialize, deserialize, onModelLoaded)

import Uuid exposing (Uuid, uuidGenerator)
import Random.Pcg exposing (Seed, step)
import Json.Encode exposing (encode, string, object, bool, Value)
import Json.Decode exposing (decodeString, field)
import Types exposing (..)
import Debug


port setLocalStorage : String -> Cmd msg


port getLocalStorage : () -> Cmd msg


port onModelLoaded : (Maybe String -> msg) -> Sub msg



-- Finders


allTasks : Model -> List Task
allTasks model =
    List.concatMap (\g -> g.tasks) model.groups


draggedTask : Model -> Maybe Task
draggedTask model =
    List.head (List.filter (\t -> t.isDragging == True) (allTasks model))


isInGroup : Task -> Group -> Bool
isInGroup task group =
    List.any (\t -> t.uuid == task.uuid) group.tasks


parentGroup : Task -> Model -> Maybe Group
parentGroup task model =
    List.head (List.filter (isInGroup task) model.groups)



-- Updating


newGroup : Seed -> ( Group, Seed )
newGroup seed =
    let
        ( uuid, newSeed ) =
            step uuidGenerator seed
    in
        ( { uuid = uuid
          , title = ""
          , tasks = []
          }
        , newSeed
        )


newTask : Seed -> ( Task, Seed )
newTask seed =
    let
        ( uuid, newSeed ) =
            step uuidGenerator seed
    in
        ( { uuid = uuid
          , description = ""
          , isDone = False
          , isDragging = False
          }
        , newSeed
        )


updateGroupTask : Group -> Task -> Group
updateGroupTask group newTask =
    let
        updateTask task =
            if task.uuid == newTask.uuid then
                newTask
            else
                task
    in
        { group | tasks = (List.map updateTask group.tasks) }


updateTask : Model -> Group -> Task -> Model
updateTask model group newTask =
    newTask
        |> updateGroupTask group
        |> updateGroup model


addNewTask : Model -> Group -> ( Model, Task )
addNewTask model group =
    let
        ( task, seed ) =
            newTask model.seed
    in
        ( { group | tasks = group.tasks ++ [ task ] }
            |> updateGroup model
            |> updateSeed seed
            |> updateFocus task
        , task
        )


moveTaskToGroup : Model -> Group -> Group -> Task -> Model
moveTaskToGroup model fromGroup group task =
    let
        newModel =
            if fromGroup == group then
                updateTask model group task
            else
                (group.tasks ++ [ task ])
                    |> asTasksIn group
                    |> updateGroup model
                    |> removeTask task fromGroup
    in
        newModel


removeTask : Task -> Group -> Model -> Model
removeTask task group model =
    (List.filter (\aTask -> task.uuid /= aTask.uuid) group.tasks)
        |> asTasksIn group
        |> updateGroup model


updateGroup : Model -> Group -> Model
updateGroup model newGroup =
    let
        updateGroup group =
            if group.uuid == newGroup.uuid then
                newGroup
            else
                group
    in
        { model | groups = List.map updateGroup model.groups }


removeGroup : Model -> Group -> Model
removeGroup model group =
    { model | groups = (List.filter (\aGrp -> group.uuid /= aGrp.uuid) model.groups) }


addNewGroup : Model -> Maybe Group -> Model
addNewGroup model preceedingGroup =
    let
        ( group, seed ) =
            newGroup model.seed

        groups =
            case preceedingGroup of
                Just preceedingGroup ->
                    List.concatMap
                        (\g ->
                            if g == preceedingGroup then
                                [ preceedingGroup, group ]
                            else
                                [ g ]
                        )
                        model.groups

                Nothing ->
                    [ group ]
    in
        { model | groups = groups, seed = seed }


updateSeed : Seed -> Model -> Model
updateSeed seed model =
    { model | seed = seed }


updateFocus : Task -> Model -> Model
updateFocus task model =
    { model | focusedTaskUuid = Just task.uuid }


asTasksIn : Group -> List Task -> Group
asTasksIn group tasks =
    { group | tasks = tasks }


dropDraggedTaskInto : Group -> Model -> Model
dropDraggedTaskInto group model =
    case (draggedTask model) of
        Nothing ->
            model

        Just task ->
            case (parentGroup task model) of
                Nothing ->
                    model

                Just fromGroup ->
                    moveTaskToGroup model fromGroup group (dropTask task)


dropTask : Task -> Task
dropTask task =
    { task | isDragging = False }


dropTaskIn : Task -> Model -> Model
dropTaskIn task model =
    case (parentGroup task model) of
        Nothing ->
            model

        Just group ->
            updateTask model group (dropTask task)


dropAllTasks : Model -> Model
dropAllTasks model =
    List.foldl (dropTaskIn) model (allTasks model)



-- Loading


loadModel : () -> Cmd msg
loadModel () =
    getLocalStorage ()


taskFromJson : Json.Decode.Decoder Task
taskFromJson =
    Json.Decode.map4 Task
        (field "uuid" Uuid.decoder)
        (field "description" Json.Decode.string)
        (field "isDone" Json.Decode.bool)
        (field "isDragging" Json.Decode.bool)


groupFromJson : Json.Decode.Decoder Group
groupFromJson =
    Json.Decode.map3 Group
        (field "uuid" Uuid.decoder)
        (field "title" Json.Decode.string)
        (field "tasks" (Json.Decode.list taskFromJson))


fromJson : Json.Decode.Decoder Model
fromJson =
    Json.Decode.map3 Model
        (field "groups" (Json.Decode.list groupFromJson))
        (field "seed" Random.Pcg.fromJson)
        (field "focusedTaskUuid" (Json.Decode.nullable Uuid.decoder))


deserialize : Maybe String -> Model -> Model
deserialize jsonStr model =
    case jsonStr of
        Nothing ->
            model

        Just jsonStr ->
            case (decodeString fromJson jsonStr) of
                Ok loadedModel ->
                    loadedModel

                Err str ->
                    let
                        _ =
                            Debug.log "Error" str
                    in
                        { model | groups = [] }



-- Saving


saveModel : Model -> Cmd msg
saveModel model =
    setLocalStorage (serialize model)


taskJson : Task -> Value
taskJson task =
    object
        [ ( "uuid", Uuid.encode task.uuid )
        , ( "description", string task.description )
        , ( "isDone", bool task.isDone )
        , ( "isDragging", bool False )
        ]


groupJson : Group -> Value
groupJson group =
    object
        [ ( "uuid", Uuid.encode group.uuid )
        , ( "title", string group.title )
        , ( "tasks", Json.Encode.list (List.map taskJson group.tasks) )
        ]


toJson : Model -> Value
toJson model =
    let
        uuidEncoder =
            case model.focusedTaskUuid of
                Nothing ->
                    Json.Encode.null

                Just uuid ->
                    Uuid.encode uuid
    in
        object
            [ ( "groups", Json.Encode.list (List.map groupJson model.groups) )
            , ( "seed", Random.Pcg.toJson model.seed )
            , ( "focusedTaskUuid", Json.Encode.null )
            ]


serialize : Model -> String
serialize model =
    encode 0 (toJson model)
