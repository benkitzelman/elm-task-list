port module Model exposing (addNewGroup, addNewTask, deserialize, loadModel, onModelLoaded, removeGroup, removeTask, saveModel, serialize, updateGroup, updateTask)

import Debug
import Json.Decode exposing (decodeString, field)
import Json.Encode exposing (Value, bool, encode, object, string)
import Random.Pcg exposing (Seed, step)
import Types exposing (..)
import Uuid exposing (Uuid, uuidGenerator)


port setLocalStorage : String -> Cmd msg


port getLocalStorage : () -> Cmd msg


port onModelLoaded : (Maybe String -> msg) -> Sub msg



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
      , isFocused = True
      , isDone = False
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
    { group | tasks = List.map updateTask group.tasks }


updateTask : Model -> Group -> Task -> Model
updateTask model group newTask =
    newTask
        |> updateGroupTask group
        |> updateGroup model


addNewTask : Model -> Group -> Model
addNewTask model group =
    let
        ( task, seed ) =
            newTask model.seed
    in
    { group | tasks = group.tasks ++ [ task ] }
        |> updateGroup model
        |> updateSeed seed


removeTask : Model -> Group -> Task -> Model
removeTask model group task =
    List.filter (\aTask -> task.uuid /= aTask.uuid) group.tasks
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
    { model | groups = List.filter (\aGrp -> group.uuid /= aGrp.uuid) model.groups }


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


asTasksIn : Group -> List Task -> Group
asTasksIn group tasks =
    { group | tasks = tasks }



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
        (field "isFocused" Json.Decode.bool)


groupFromJson : Json.Decode.Decoder Group
groupFromJson =
    Json.Decode.map3 Group
        (field "uuid" Uuid.decoder)
        (field "title" Json.Decode.string)
        (field "tasks" (Json.Decode.list taskFromJson))


fromJson : Json.Decode.Decoder Model
fromJson =
    Json.Decode.map2 Model
        (field "groups" (Json.Decode.list groupFromJson))
        (field "seed" Random.Pcg.fromJson)


deserialize : Maybe String -> Model -> Model
deserialize jsonStr model =
    case jsonStr of
        Nothing ->
            model

        Just jsonStr ->
            case decodeString fromJson jsonStr of
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
        , ( "isFocused", bool task.isFocused )
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
    object
        [ ( "groups", Json.Encode.list (List.map groupJson model.groups) )
        , ( "seed", Random.Pcg.toJson model.seed )
        ]


serialize : Model -> String
serialize model =
    encode 0 (toJson model)
