module Types exposing (..)

import Dom exposing (..)
import Mouse
import Uuid exposing (Uuid)
import Random.Pcg exposing (Seed)


type alias Task =
    { uuid : Uuid, description : String, isDone : Bool, isDragging : Bool }


type alias Group =
    { uuid : Uuid, title : String, tasks : List Task, isDragging : Bool }


type alias Model =
    { groups : List Group, seed : Seed, mouseCoords : Maybe Mouse.Position, focusedTaskUuid : Maybe Uuid, showImportModal : Bool }


type Position
    = Before
    | After


type Msg
    = Ignore (Result Error ())
    | GroupTitle Group String
    | TaskDescription Group Task String
    | TaskIsDone Group Task
    | TaskRemove Group Task
    | TaskNew Group
    | TaskDrag Group Task
    | Drop (Maybe Group)
    | GroupRemove Group
    | GroupNew (Maybe Group)
    | GroupDrag Group
    | ShowImport
    | ImportFile String
    | OnImported (Maybe String)
    | CloseImport
    | OnLoad (Maybe String)
    | MouseMove Mouse.Position
