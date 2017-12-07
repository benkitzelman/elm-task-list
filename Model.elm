port module Model exposing (Task, Group, Model, loadModel, saveModel, serialize, deserialize, onModelLoaded, updateGroupTask, updateTask, updateGroup, updateSeed, asTasksIn)

import Uuid exposing (Uuid)
import Random.Pcg exposing (Seed)
import Json.Encode exposing (encode, string, object, bool, Value)
import Json.Decode exposing (decodeString, field)


type alias Task =
    { uuid : Uuid, description : String, isDone : Bool, isEditing : Bool }


type alias Group =
    { uuid : Uuid, title : String, tasks : List Task }


type alias Model =
    { groups : List Group, seed : Seed }


port setLocalStorage : String -> Cmd msg


port getLocalStorage : () -> Cmd msg


port onModelLoaded : (Maybe String -> msg) -> Sub msg



-- Updating


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
        (field "isEditing" Json.Decode.bool)


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


deserialize : String -> Result String Model
deserialize jsonStr =
    decodeString fromJson jsonStr



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
        , ( "isEditing", bool task.isEditing )
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
