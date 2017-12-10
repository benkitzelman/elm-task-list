module Types exposing (..)

import Random.Pcg exposing (Seed)
import Uuid exposing (Uuid)


type Msg
    = GroupTitle Group String
    | TaskDescription Group Task String
    | TaskIsDone Group Task
    | TaskRemove Group Task
    | TaskSave Group Task
    | TaskNew Group
    | GroupRemove Group
    | GroupNew (Maybe Group)
    | Import
    | OnLoad (Maybe String)


type alias Task =
    { uuid : Uuid, description : String, isDone : Bool, isFocused : Bool }


type alias Group =
    { uuid : Uuid, title : String, tasks : List Task }


type alias Model =
    { groups : List Group, seed : Seed }
