module App exposing (..)

import Html exposing (Html)
import Model exposing (..)
import Uuid exposing (Uuid, uuidGenerator)
import Random.Pcg exposing (initialSeed, step)
import Debug
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Color exposing (Color)


type Msg
    = GroupTitle Group String
    | TaskDescription Group Task String
    | TaskIsDone Group Task
    | TaskEdit Group Task
    | TaskRemove Group Task
    | TaskSave Group Task
    | TaskNew Group
    | GroupRemove Group
    | GroupNew
    | Load
    | OnLoad (Maybe String)


main : Program Int Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Int -> ( Model, Cmd Msg )
init flags =
    let
        ( groupUuid, initSeed ) =
            step uuidGenerator (initialSeed flags)

        ( taskUuid, seed ) =
            step uuidGenerator initSeed
    in
        ( { seed = seed
          , groups =
                [ { uuid = groupUuid
                  , title = ""
                  , tasks =
                        [ { uuid = taskUuid
                          , description = ""
                          , isEditing = True
                          , isDone = False
                          }
                        ]
                  }
                ]
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
                    updateTask model group { task | isDone = (not task.isDone) }
            in
                ( newModel, saveModel newModel )

        TaskEdit group task ->
            let
                newModel =
                    updateTask model group { task | isEditing = True }
            in
                ( newModel, saveModel newModel )

        TaskSave group task ->
            let
                newModel =
                    updateTask model group { task | isEditing = False }
            in
                ( newModel, saveModel newModel )

        TaskRemove group task ->
            let
                newModel =
                    (List.filter (\aTask -> task.uuid /= aTask.uuid) group.tasks)
                        |> asTasksIn group
                        |> updateGroup model
            in
                ( newModel, saveModel newModel )

        TaskNew group ->
            let
                ( uuid, seed ) =
                    step uuidGenerator model.seed

                tasks =
                    group.tasks
                        ++ [ { uuid = uuid
                             , description = ""
                             , isEditing = True
                             , isDone = False
                             }
                           ]

                newModel =
                    { group | tasks = tasks }
                        |> updateGroup model
                        |> updateSeed seed
            in
                ( newModel, saveModel newModel )

        Load ->
            ( model, loadModel () )

        GroupNew ->
            let
                ( uuid, seed ) =
                    step uuidGenerator model.seed

                groups =
                    model.groups
                        ++ [ { uuid = uuid
                             , title = ""
                             , tasks = []
                             }
                           ]

                newModel =
                    { model | groups = groups, seed = seed }
            in
                ( newModel, saveModel newModel )

        GroupRemove group ->
            let
                newModel =
                    { model | groups = (List.filter (\aGrp -> group.uuid /= aGrp.uuid) model.groups) }
            in
                ( newModel, saveModel newModel )

        OnLoad jsonStr ->
            case jsonStr of
                Nothing ->
                    ( model, Cmd.none )

                Just jsonStr ->
                    case (deserialize jsonStr) of
                        Ok loadedModel ->
                            ( loadedModel, Cmd.none )

                        Err str ->
                            let
                                _ =
                                    Debug.log "Error" str
                            in
                                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onModelLoaded OnLoad


type Styles
    = None
    | JobLog
    | Header
    | Title
    | Group
    | TitleField
    | TextInput
    | Button
    | ImageButton
    | TaskDone
    | TaskPending
    | DoneIndicator
    | PendingIndicator


colorHighlight : Color
colorHighlight =
    Color.rgb 240 223 156


colorDefault : Color
colorDefault =
    Color.white


colorBackground : Color
colorBackground =
    Color.darkCharcoal


colorBackgroundAlt : Color
colorBackgroundAlt =
    Color.lightCharcoal


colorTransparent : Color
colorTransparent =
    Color.rgba 0 0 0 0


buttonStyle : List (Property class variation)
buttonStyle =
    --[ Border.all 1 -- set all border widths to 1 px.
    [ Color.background colorTransparent
    , Color.text colorHighlight
    , hover
        [ Color.text (Color.rgb 219 80 96)
        ]
    ]


commonSpacing : List (Attribute variation msg)
commonSpacing =
    [ spacing 10 ]


textInput : List (Property class variation)
textInput =
    [ Color.background colorTransparent
    , Color.text colorDefault
    ]


taskStyle : List (Property class variation)
taskStyle =
    [ Border.bottom 1
    , Color.border (Color.rgba 255 255 255 0.2)
    ]


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None [] -- It's handy to have a blank style
        , style JobLog
            [ Color.text colorDefault
            , Color.background colorBackground
            , Color.border Color.lightGrey
            , Font.typeface [ Font.sansSerif ]
            , Font.size 14
            , Font.lineHeight 1.3
            ]
        , style Header
            [ Border.bottom 1
            , Color.border colorBackgroundAlt
            ]
        , style Title
            [ Font.size 20
            ]
        , style Group
            []
        , style TaskDone
            (taskStyle
                ++ [ Color.background (Color.rgb 67 74 78)
                   , Color.text Color.grey
                   ]
            )
        , style TaskPending
            (taskStyle ++ [])
        , style TitleField
            (textInput ++ [ Font.size 18 ])
        , style TextInput
            textInput
        , style Button
            buttonStyle
        , style ImageButton
            [ Color.background colorTransparent
            ]
        , style DoneIndicator
            [ Color.background (Color.rgb 219 80 96) ]
        , style PendingIndicator
            [ Color.background (Color.rgb 28 155 198) ]
        ]


inputField : Styles -> String -> (String -> Msg) -> String -> Element Styles variation Msg
inputField style value msg placeholder =
    Input.text style
        [ paddingTop 15, paddingBottom 15 ]
        { onChange = msg
        , value = value
        , options = []
        , label =
            Input.placeholder
                { label = (Input.hiddenLabel placeholder)
                , text = placeholder
                }
        }


type ButtonContent
    = Image String
    | Text String


btn : Msg -> ButtonContent -> Element Styles variation Msg
btn msg content =
    let
        ( style, element ) =
            case content of
                Image str ->
                    ( ImageButton, image None [] { src = str, caption = "" } )

                Text str ->
                    ( Button, text str )
    in
        button style [ onClick msg, paddingTop 3, paddingBottom 3, paddingLeft 8, paddingRight 8 ] element


indicator : Bool -> Element Styles variation Msg
indicator isDone =
    let
        style =
            if isDone then
                DoneIndicator
            else
                PendingIndicator
    in
        el style [ width (px 3) ] (text "")


view : Model -> Html Msg
view model =
    viewport stylesheet <|
        column JobLog
            [ height (percent 100), width (percent 100), center ]
            [ column None
                (commonSpacing ++ [ width (px 800) ])
                [ row Header
                    [ spread, paddingTop 10, paddingBottom 10 ]
                    [ h1 Title [] (text "Job Log")
                    , btn Load (Text "Load")
                    ]
                , column None commonSpacing (List.map renderGroup model.groups)
                ]
            ]


renderGroup : Group -> Element Styles variation Msg
renderGroup group =
    let
        renderTask task =
            if task.isDone == False then
                editTask TaskPending group task
            else
                viewTask TaskDone group task
    in
        column Group
            commonSpacing
            [ row None
                (commonSpacing ++ [ spread ])
                [ inputField TitleField group.title (GroupTitle group) "Enter group title..."
                , btn (GroupRemove group) (Image "assets/images/bin_25_25.png")
                ]
            , column None
                []
                (List.map renderTask group.tasks)
            , row None
                [ spread, paddingBottom 10 ]
                [ btn GroupNew (Text "+ Add Group")
                , btn (TaskNew group) (Text "+ Add Task")
                ]
            ]


editTask : Styles -> Group -> Task -> Element Styles variation Msg
editTask style group task =
    row style
        commonSpacing
        [ indicator task.isDone
        , inputField TextInput task.description (TaskDescription group task) "Enter task description..."
        , row None
            commonSpacing
            [ btn (TaskIsDone group task) (Text "Complete")
            , btn (TaskRemove group task) (Text "Remove")
            ]
        ]


viewTask : Styles -> Group -> Task -> Element Styles variation Msg
viewTask style group task =
    row style
        commonSpacing
        [ indicator task.isDone
        , el None [ width fill, paddingTop 15, paddingBottom 15 ] (text task.description)
        , btn (TaskIsDone group task) (Text "Edit")
        ]
