port module Model exposing (setGroupDropPosition, draggedTask, dropDragged, dropAll, dropTaskIn, moveTaskToGroup, parentGroup, addNewGroup, addNewTask, removeTask, removeGroup, updateTask, updateGroup, loadModel, saveModel, serialize, deserialize, onModelLoaded, readSelectedFileFromInput, onFileImported)

import Uuid exposing (Uuid, uuidGenerator)
import Random.Pcg exposing (Seed, step)
import Json.Encode exposing (encode, string, object, bool, int, Value)
import Json.Decode exposing (decodeString, field)
import Types exposing (..)
import Debug


port setLocalStorage : String -> Cmd msg


port getLocalStorage : () -> Cmd msg


port onModelLoaded : (Maybe String -> msg) -> Sub msg


port readSelectedFileFromInput : String -> Cmd msg


port onFileImported : (Maybe String -> msg) -> Sub msg



-- Finders


allTasks : Model -> List Task
allTasks model =
    List.concatMap (\g -> g.tasks) model.groups


draggedTask : Model -> Maybe Task
draggedTask model =
    List.head (List.filter (\t -> t.isDragging == True) (allTasks model))


draggedGroup : Model -> Maybe Group
draggedGroup model =
    List.head (List.filter (\t -> t.isDragging == True) model.groups)


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
          , isDragging = False
          , dropPosition = Nothing
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


removeGroup : Group -> Model -> Model
removeGroup group model =
    { model | groups = (List.filter (\aGrp -> group.uuid /= aGrp.uuid) model.groups) }



-- TODO: should be a lib that handles this


insertGroup : Position -> Group -> Group -> List Group -> List Group
insertGroup position preceedingGroup newGroup list =
    let
        insert g =
            if g == preceedingGroup then
                case position of
                    Before ->
                        [ newGroup, g ]

                    After ->
                        [ g, newGroup ]
            else
                [ g ]
    in
        List.concatMap insert list


addNewGroup : Model -> Maybe Group -> Model
addNewGroup model preceedingGroup =
    let
        ( group, seed ) =
            newGroup model.seed

        groups =
            case preceedingGroup of
                Just preceedingGroup ->
                    insertGroup After preceedingGroup group model.groups

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
dropDraggedTaskInto toGroup model =
    case (draggedTask model) of
        Nothing ->
            model

        Just task ->
            case (parentGroup task model) of
                Nothing ->
                    model

                Just fromGroup ->
                    moveTaskToGroup model fromGroup toGroup (dropTask task)


dropDraggedGroup : Position -> Group -> Model -> Model
dropDraggedGroup position group model =
    let
        insert dGroup model =
            { model | groups = (insertGroup position group dGroup model.groups) }
    in
        case (draggedGroup model) of
            Nothing ->
                model

            Just dGroup ->
                let
                    newModel =
                        if dGroup == group then
                            model
                        else
                            model
                                |> removeGroup dGroup
                                |> insert (dropGroup dGroup)
                in
                    newModel


dropDragged : Group -> Model -> Model
dropDragged group model =
    let
        pos =
            case (getDropPosition model) of
                Nothing ->
                    After

                Just pos ->
                    pos
    in
        model
            |> dropDraggedGroup pos group
            |> dropDraggedTaskInto group


getDropPosition : Model -> Maybe Position
getDropPosition model =
    let
        dropPos group pos =
            if (group.dropPosition /= Nothing) then
                group.dropPosition
            else
                pos
    in
        List.foldl dropPos (Just After) model.groups


setGroupDropPosition : Position -> Group -> Model -> Model
setGroupDropPosition pos group model =
    let
        clearGroupDropPosition group model =
            updateGroup model { group | dropPosition = Nothing }

        cleanModel =
            List.foldl clearGroupDropPosition model model.groups
    in
        case (draggedGroup cleanModel) of
            Nothing ->
                cleanModel

            Just _ ->
                updateGroup cleanModel { group | dropPosition = Just pos }



--TODO: There has to be a way to use types to dry this up


dropTask : Task -> Task
dropTask task =
    { task | isDragging = False }


dropGroup : Group -> Group
dropGroup group =
    { group | isDragging = False, dropPosition = Nothing }


dropTaskIn : Task -> Model -> Model
dropTaskIn task model =
    case (parentGroup task model) of
        Nothing ->
            model

        Just group ->
            updateTask model group (dropTask task)


dropGroupIn : Group -> Model -> Model
dropGroupIn group model =
    if group.isDragging then
        updateGroup model (dropGroup group)
    else
        model


dropAllTasks : Model -> Model
dropAllTasks model =
    List.foldl dropTaskIn model (allTasks model)


dropAllGroups : Model -> Model
dropAllGroups model =
    List.foldl dropGroupIn model model.groups


dropAll : Model -> Model
dropAll model =
    model
        |> dropAllTasks
        |> dropAllGroups



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
    Json.Decode.map5 Group
        (field "uuid" Uuid.decoder)
        (field "title" Json.Decode.string)
        (field "tasks" (Json.Decode.list taskFromJson))
        (field "isDragging" Json.Decode.bool)
        (field "dropPosition" (Json.Decode.null Nothing))


fromJson : Json.Decode.Decoder Model
fromJson =
    Json.Decode.map5 Model
        (field "groups" (Json.Decode.list groupFromJson))
        (field "seed" Random.Pcg.fromJson)
        (field "mouseCoords" (Json.Decode.null Nothing))
        (field "focusedTaskUuid" (Json.Decode.nullable Uuid.decoder))
        (field "showImportModal" Json.Decode.bool)


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
        , ( "isDragging", bool False )
        , ( "dropPosition", Json.Encode.null )
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
            , ( "mouseCoords", Json.Encode.null )
            , ( "focusedTaskUuid", Json.Encode.null )
            , ( "showImportModal", bool False )
            ]


serialize : Model -> String
serialize model =
    encode 0 (toJson model)
