module App exposing (..)

import Html exposing (Html)
import Model exposing (..)
import Random.Pcg exposing (initialSeed)
import Types exposing (..)
import View


main : Program Int Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = View.view
        , subscriptions = subscriptions
        }


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { seed = initialSeed flags
      , groups = []
      }
    , loadModel ()
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GroupTitle group title ->
            let
                newModel =
                    updateGroup model { group | title = title }
            in
            ( newModel, saveModel newModel )

        TaskDescription group task desc ->
            let
                newModel =
                    updateTask model group { task | description = desc }
            in
            ( newModel, saveModel newModel )

        TaskIsDone group task ->
            let
                newModel =
                    updateTask model group { task | isDone = not task.isDone }
            in
            ( newModel, saveModel newModel )

        TaskSave group task ->
            let
                newModel =
                    updateTask model group { task | isFocused = False }
            in
            ( newModel, saveModel newModel )

        TaskRemove group task ->
            let
                newModel =
                    removeTask model group task
            in
            ( newModel, saveModel newModel )

        TaskNew group ->
            let
                newModel =
                    addNewTask model group
            in
            ( newModel, saveModel newModel )

        Import ->
            ( model, Cmd.none )

        GroupNew preceedingGroup ->
            let
                newModel =
                    addNewGroup model preceedingGroup
            in
            ( newModel, saveModel newModel )

        GroupRemove group ->
            let
                newModel =
                    removeGroup model group
            in
            ( newModel, saveModel newModel )

        OnLoad jsonStr ->
            let
                newModel =
                    deserialize jsonStr model
            in
            ( newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onModelLoaded OnLoad
