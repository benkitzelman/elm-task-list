module View exposing (..)

import Color exposing (Color)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Model exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Types exposing (..)


view : Model -> Html Msg
view model =
    let
        content =
            if List.isEmpty model.groups then
                [ btn (GroupNew Nothing) (Text "+ Add Group") ]
            else
                List.map renderGroup model.groups
    in
    viewport stylesheet <|
        column JobLog
            [ height (percent 100), width (percent 100), center ]
            [ row
                Header
                [ width (percent 100), center ]
                [ row None
                    (commonSpacing ++ [ width (px 800), spread, paddingXY 0 10 ])
                    [ h1 Title [] (text "Job Log")
                    , row None
                        (commonSpacing ++ [ alignRight ])
                        [ btn Import (Text "Import")
                        , exportBtn model
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
            [ btn (GroupNew (Just group)) (Text "+ Add Group")
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
        , el None [ width fill, paddingXY 0 15 ] (text task.description)
        , btn (TaskIsDone group task) (Text "Edit")
        ]



-- View -----------------------


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


inputField : Styles -> String -> (String -> msg) -> String -> Element Styles variation msg
inputField style value msg placeholder =
    Input.text style
        [ paddingXY 0 15 ]
        { onChange = msg
        , value = value
        , options = []
        , label =
            Input.placeholder
                { label = Input.hiddenLabel placeholder
                , text = placeholder
                }
        }


type ButtonContent
    = Image String
    | Text String


btn : msg -> ButtonContent -> Element Styles variation msg
btn msg content =
    let
        ( style, element ) =
            case content of
                Image str ->
                    ( ImageButton, image None [] { src = str, caption = "" } )

                Text str ->
                    ( Button, text str )
    in
    button style [ onClick msg, paddingXY 8 3 ] element


exportBtn : Model -> Element Styles variation msg
exportBtn model =
    let
        src =
            "data:application/octet-stream," ++ serialize model
    in
    downloadAs { src = src, filename = "job-log.json" } <|
        el Button [ paddingXY 8 3 ] (text "Export")


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
