module Views exposing (view)

import Types exposing (..)
import Model exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Color exposing (Color)


type Styles
    = None
    | JobLog
    | Header
    | Content
    | Title
    | Group
    | TitleField
    | TextInput
    | Button
    | ImageButton
    | MoveHandle
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
            [ Color.background (Color.rgb 25 25 25)
            ]
        , style Content
            []
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
            taskStyle
        , style TitleField
            (textInput ++ [ Font.size 18 ])
        , style TextInput
            textInput
        , style Button
            buttonStyle
        , style ImageButton
            [ Color.background colorTransparent
            ]
        , style MoveHandle
            (buttonStyle ++ [ Style.cursor "move !important" ])
        , style DoneIndicator
            [ Color.background (Color.rgb 219 80 96) ]
        , style PendingIndicator
            [ Color.background (Color.rgb 28 155 198) ]
        ]


inputField : Styles -> String -> String -> (String -> msg) -> List (Input.Option Styles variation msg) -> String -> Element Styles variation msg
inputField style idStr value msg options placeholder =
    Input.text style
        [ id idStr, paddingXY 0 15 ]
        { onChange = msg
        , value = value
        , options = options
        , label =
            Input.placeholder
                { label = (Input.hiddenLabel placeholder)
                , text = placeholder
                }
        }


type ButtonType
    = Image String
    | Text String
    | Move String


btn : ButtonType -> List (Attribute variation msg) -> Element Styles variation msg
btn buttonType attrs =
    let
        ( style, element ) =
            case buttonType of
                Image str ->
                    ( ImageButton, image None [] { src = str, caption = "" } )

                Text str ->
                    ( Button, text str )

                Move str ->
                    ( MoveHandle, text str )
    in
        button style (attrs ++ [ paddingXY 8 3 ]) element


exportBtn : Model -> Element Styles variation msg
exportBtn model =
    let
        src =
            "data:application/octet-stream," ++ (serialize model)
    in
        downloadAs { src = src, filename = "job-log.json" } <|
            (el Button [ paddingXY 8 3 ] (text "Export"))


indicator : Bool -> Element Styles variation msg
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
    let
        render =
            renderGroup model

        content =
            if List.isEmpty model.groups then
                [ btn (Text "+ Add Group") [ onClick (GroupNew Nothing) ] ]
            else
                (List.map render model.groups)
    in
        viewport stylesheet <|
            column JobLog
                [ height (percent 100), width (percent 100), center, onMouseUp (TaskDrop Nothing) ]
                [ row
                    Header
                    [ width (percent 100), center ]
                    [ row None
                        (commonSpacing ++ [ width (px 800), spread, paddingXY 0 10 ])
                        [ h1 Title [] (text "Job Log")
                        , row None
                            (commonSpacing ++ [ alignRight ])
                            [ btn (Text "Import") [ onClick Import ]
                            , (exportBtn model)
                            ]
                        ]
                    ]
                , row Content
                    [ width (percent 100), center, yScrollbar ]
                    [ column None
                        (commonSpacing ++ [ width (px 800) ])
                        [ column None commonSpacing content
                        ]
                    ]
                ]


renderGroup : Model -> Group -> Element Styles variation Msg
renderGroup model group =
    let
        renderTask task =
            if task.isDone == False then
                editTask TaskPending model group task
            else
                viewTask TaskDone group task
    in
        column Group
            (commonSpacing ++ [ onMouseUp (TaskDrop (Just group)) ])
            [ row None
                (commonSpacing ++ [ spread ])
                [ inputField TitleField (toString group.uuid) group.title (GroupTitle group) [] "Enter group title..."
                , btn (Image "assets/images/bin_25_25.png") [ onClick (GroupRemove group) ]
                ]
            , column None
                []
                (List.map renderTask group.tasks)
            , row None
                [ spread, paddingBottom 10 ]
                [ btn (Text "+ Add Group") [ onClick (GroupNew (Just group)) ]
                , btn (Text "+ Add Task") [ onClick (TaskNew group) ]
                ]
            ]


editTask : Styles -> Model -> Group -> Task -> Element Styles variation Msg
editTask style model group task =
    let
        options =
            case model.focusedTaskUuid of
                Nothing ->
                    []

                Just uuid ->
                    if task.uuid == uuid then
                        [ Input.focusOnLoad ]
                    else
                        []
    in
        row style
            commonSpacing
            [ indicator task.isDone
            , inputField TextInput (toString task.uuid) task.description (TaskDescription group task) options "Enter task description..."
            , row None
                commonSpacing
                [ btn (Move "Move") [ onMouseDown (TaskDrag group task) ]
                , btn (Text "Complete") [ onClick (TaskIsDone group task) ]
                , btn (Text "Remove") [ onClick (TaskRemove group task) ]
                ]
            ]


viewTask : Styles -> Group -> Task -> Element Styles variation Msg
viewTask style group task =
    row style
        commonSpacing
        [ indicator task.isDone
        , el None [ width fill, paddingXY 0 15 ] (text task.description)
        , btn (Text "Edit") [ onClick (TaskIsDone group task) ]
        ]