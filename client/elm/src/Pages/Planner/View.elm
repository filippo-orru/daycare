module Pages.Planner.View exposing (view, viewAttribute, viewAttributes, viewDay, viewDays, viewGoal, viewGoals, viewResponseHttpError, viewSidebar, viewSyncIndicator, viewTask, viewTasks)

import Array exposing (Array)
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onDoubleClick, onInput, onMouseOut, onMouseOver, onSubmit)
import Html.Extra as Html
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Http
import Json.Decode as D
import Json.Encode as E
import Pages.Planner.Types as Model exposing (..)
import Route


view : StateModel -> List (Html StateMsg)
view model_ =
    case model_ of
        Guest model ->
            [ div [ class "planner container" ]
                [ div [ class "planner box" ]
                    [ p [ class "planner error-text" ] [ text "You have to be logged in to view this page." ]
                    , a [ class "planner link-button", Route.href Route.Login ] [ text "Login" ]
                    ]
                ]
            ]

        Loggedin model ->
            [ div [ class "planner container" ]
                [ let
                    loading tpl =
                        p [ class "planner loading-text", title <| Tuple.second tpl ] [ text <| Tuple.first tpl ]

                    --"Loading content...""
                    viewElementStatus loadElement =
                        case loadElement of
                            Nothing ->
                                ( "...", " green" )

                            Just (Err err) ->
                                viewResponseHttpError err

                            Just (Ok _) ->
                                ( "✓", " green" )
                  in
                  div [ class "planner box" ]
                    [ loading ( "Loading User..." ++ (Tuple.first <| viewElementStatus model.user), Tuple.second <| viewElementStatus model.user )
                    , loading ( "Loading Days..." ++ (Tuple.first <| viewElementStatus model.days), Tuple.second <| viewElementStatus model.days )
                    ]
                ]
            ]

        Loaded model ->
            List.map (Html.map LoadedMsg)
                [ div [ class "planner header" ] [ text model.user.email ]
                , div [ class "planner wrapper" ]
                    [ viewSidebar model.viewState model.user
                    , div [ class "planner planner-body" ]
                        [ viewDays model.days model.viewState
                        ]
                    ]
                ]


viewSidebar : ViewState -> User -> Html LoadedMsg
viewSidebar viewState user =
    div
        [ class <|
            "planner sidebar-wrapper"
                ++ (if viewState.sidebarExpanded then
                        " show"

                    else
                        " hide"
                   )
        ]
        [ div [ class "planner sidebar" ]
            [ div [ class "planner sidebar-header noselect" ]
                [ i [ class "fas fa-user-circle", title user.email ] []
                , h2 [ class "planner sidebar-header-text" ] [ text "daycare" ]
                , viewSyncIndicator viewState.loading
                ]
            , hr [ class "planner sidebar" ] []
            , div [ class "planner sidebar-body" ]
                [ -- text ("email: " ++ user.email)
                  viewAttributes user viewState
                ]
            , button [ class "planner sidebar-expand", onClick ToggleSidebar ] [ text ">" ]
            ]
        ]


viewSyncIndicator : Maybe ViewLoadState -> Html LoadedMsg
viewSyncIndicator loading =
    div [ class "planner sync-indicator" ]
        [ case loading of
            Just ViewLLoading ->
                i
                    [ class "planner fas fa-spinner"
                    , title "Loading..."
                    ]
                    []

            Just (ViewLError err) ->
                i
                    [ class "planner fas fa-exclamation-circle"
                    , title <| Tuple.first <| viewResponseHttpError err
                    ]
                    []

            _ ->
                i
                    [ class "planner fas fa-check-circle"
                    , title "Up to date!"
                    ]
                    []
        ]


viewDays : Array RangeDay -> ViewState -> Html LoadedMsg
viewDays days viewState =
    div [ class "planner days" ] <|
        -- (case daysLs of
        --     DaysLSuccess days ->
        (Array.indexedMap (viewDay viewState) days
            |> Array.toList
        )
            ++ [ button [ onClick LoadMoreDays, class "planner loadmoredays" ] [ text "more" ]
               ]



-- ++ [ div [ class "planner day add-day" ] []
{- [ i [ class "planner day-add-button fas fa-plus-circle" ] []

          -- text "add day lol" ]
          ]
       -
       DaysLWait ->
           [ text "..." ]

       DaysLError err ->
           [ text <| "error occurred. " ++ viewResponseHttpError err ]

       _ ->
           [ text "not loading yet", button [ onClick LoadDays ] [ text "load" ] ]
   )
-}


viewDay : ViewState -> Int -> RangeDay -> Html LoadedMsg
viewDay viewState index day_ =
    case day_.day of
        Just day ->
            let
                ( ( textDesc, descClassApp ), emptyDesc ) =
                    case day.description of
                        Just "" ->
                            ( ( "Click to add description.", " add" ), "" )

                        Nothing ->
                            ( ( "Click to add description.", " add" ), "" )

                        Just other ->
                            ( ( other, "" ), other )

                viewDefaultDesc =
                    div [ class "planner day-description-wrapper", onClick (EditStart <| DayPartKey index <| DDescription emptyDesc) ]
                        [ p [ class <| "planner day-description" ++ descClassApp ] [ text textDesc ]
                        , i [ class "fas fa-pen" ] []
                        ]

                viewDescription =
                    case viewState.editing of
                        Just (DayPartKey index_ (DDescription desc)) ->
                            if index == index_ then
                                Html.form [ onSubmit EditCommit ]
                                    [ input [ id "edit-input", class "planner day-description", onKeyDown KeyDown, onInput (EditUpdate << DayPartKeyW index << Just << DDescription), value desc ] []
                                    ]

                            else
                                viewDefaultDesc

                        _ ->
                            viewDefaultDesc

                date =
                    day.date
                        |> Date.format "dd.MM.YYYY"

                adding =
                    case viewState.adding of
                        Just (DayPartKey index_ (DTask _ _)) ->
                            index == index_

                        _ ->
                            False

                editing =
                    case viewState.editing of
                        Just (DayPartKey index_ (DTask taskkey task)) ->
                            if index == index_ then
                                Just (DayPartKey index_ (DTask taskkey task))

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            div [ class " planner day" ]
                [ div
                    [ class "planner day-header noselect" ]
                    [ h4 [ class "planner day-date" ] [ text date ]
                    , viewDescription
                    ]
                , div [ class "planner day-body" ]
                    [ viewTasks index day adding editing ]
                ]

        Nothing ->
            let
                date =
                    Date.format "dd.MM.YYYY" day_.date
            in
            div [ class "planner day nocontent", onClick (AddDay index day_.date) ]
                [ div [ class "planner day-header" ]
                    [ h4 [ class "planner day-date" ] [ text date ]
                    ]
                , text "click to add day"
                ]


viewTasks : Int -> Day -> Bool -> Maybe ContentKey -> Html LoadedMsg
viewTasks index day adding editing =
    ul [ class "planner day-tasks" ]
        ((Array.indexedMap (viewTask index editing) day.tasks
            |> Array.toList
         )
            -- ++ [ div [ class "planner day-task add" ]
            ++ [ if adding then
                    li [ class "planner day-task adding" ]
                        [ Html.form [ class "planner add-task-form", onSubmit AddCommit ]
                            [ input [ id "edit-input", class "planner add-task-input", onInput AddUpdate, placeholder "task", autofocus True, autocomplete False ] [] ]
                        ]

                 else
                    li [ class "planner day-task add-task", onClick (AddStart <| DayTaskKeyW index 0 Nothing) ]
                        [ i [ class "fas fa-plus" ] []
                        ]
               ]
        )



-- |> List.reverse
--    ]


viewTask : Int -> Maybe ContentKey -> Int -> Task -> Html LoadedMsg
viewTask dayindex maybeediting taskindex task =
    let
        classCompleted =
            if task.state == TSCompleted then
                "task-done"

            else
                "task-todo"

        defaultView =
            li
                [ class <| "planner day-task noselect " ++ classCompleted
                , onClick (ToggleDayTask dayindex task taskindex)
                , onRightClick (EditStart <| DayPartKey dayindex <| DTask taskindex task)
                ]
                [ text task.name ]
    in
    case maybeediting of
        Just (DayPartKey index_ (DTask taskindex_ task_)) ->
            if taskindex == taskindex_ then
                li [ class <| "planner day-task editing" ]
                    [ Html.form [ onSubmit EditCommit, onKeyDown KeyDown ]
                        [ input
                            [ id "edit-input"
                            , class "planner day-task editing-input"
                            , style "width" <| (String.fromInt <| String.length task_.name) ++ ".5ch"
                            , value task_.name
                            , autofocus True
                            , maxlength 30
                            , minlength 3
                            , autocomplete False
                            , attribute "data-lpignore" "true"
                            , onInput (\s -> EditUpdate <| DayTaskKeyW dayindex taskindex <| Just <| DTName s)
                            ]
                            [ text "test" ]
                        ]
                    ]

            else
                defaultView

        _ ->
            defaultView



{- take Http Error and return human readable error and debugToString of error -}


viewResponseHttpError : Http.Error -> ( String, String )
viewResponseHttpError err =
    ( case err of
        -- IMPLEMENT : MAYBE HTTP ERROR -> HTTP ERROR
        Http.BadStatus code ->
            -- text ("Could not get token. Status " ++ String.fromInt code)
            case code of
                400 ->
                    "Bad Input!"

                401 ->
                    "Wrong identifier or password!"

                500 ->
                    "Server error!"

                _ ->
                    "Http error (" ++ String.fromInt code ++ ")"

        Http.NetworkError ->
            "Could not connect to server!"

        Http.BadBody _ ->
            "Return has bad format"

        _ ->
            Debug.toString err
    , Debug.toString err
    )


viewAttributes : User -> ViewState -> Html LoadedMsg
viewAttributes user viewState =
    let
        attributesList =
            user.attributes
                |> Array.indexedMap
                    (\index att -> lazy4 viewAttribute index att viewState.loading viewState.editing)
                |> Array.toList
    in
    div [ class "planner attributes" ] <|
        [ div [ class "planner attributes-header" ]
            [ h3 [ class "planner attributes-header-text" ] [ text "Attributes" ] ]
        , hr [ class "planner" ] []
        , div [ class "planner attributes-container" ] attributesList
        , div [ class "planner attribute add-attribute" ]
            [ div [ class "planner attribute-add-button", onClick (AddStart <| AttributeKeyW Nothing) ]
                [ i [ class "planner attribute-add-icon fas fa-plus" ] []
                ]
            ]
        ]


viewAttribute : Int -> Model.Attribute -> Maybe ViewLoadState -> Maybe ContentKey -> Html LoadedMsg
viewAttribute index att loading editing =
    let
        key =
            AttributeKey index att
    in
    let
        description =
            case att.description of
                Just desc ->
                    text desc

                Nothing ->
                    text ""

        editingThis =
            case editing of
                Just (AttributeKey index_ att_) ->
                    if index == index_ then
                        Just att_

                    else
                        Nothing

                _ ->
                    Nothing
    in
    case editingThis of
        Just att_ ->
            let
                shortinput =
                    input
                        [ onInput <| (EditUpdate << AttributeKeyW << Just << AShort)
                        , value att_.short
                        , id "edit-input"
                        , class "planner attribute-short"
                        ]
                        []

                nameinput =
                    input
                        [ onInput (EditUpdate << AttributeKeyW << Just << AName)
                        , value att_.name
                        , id "edit-input"
                        , class "planner attribute-name"
                        ]
                        []

                descinput =
                    textarea
                        [ onInput <|
                            (EditUpdate << AttributeKeyW << Just << ADescription)
                        , class "planner attribute-desc"
                        ]
                        [ description ]

                -- key_ =
                --     AttributeKey index att_
                {- onMouseOver <| HoverEvent key True
                   , onMouseOut <| HoverEvent key False
                   ,
                -}
            in
            div
                [ class "planner attribute editing"
                ]
                [ Html.form [ onSubmit <| EditCommit ]
                    [ div [ class "planner attribute-editing-header" ]
                        [ shortinput
                        , nameinput

                        -- , if untouched then
                        --     {- untouched editing -}
                        --     text ""
                        --   else
                        --     {- touched editing -}
                        , div [ class "planner buttons editing" ]
                            [ button [ type_ "button", onClick (EditRemove key), class "planner fas fa-trash" ] []
                            , button [ type_ "button", onClick EditDiscard, class "planner fas fa-times" ] []
                            , button [ type_ "submit", class "planner fas fa-check" ] [] --text "⇧"
                            ]
                        ]
                    ]
                , descinput
                ]

        Nothing ->
            div
                [ class "planner attribute draggable"

                -- , id <| "att-" ++ String.fromInt index
                -- , draggable "true"
                -- , attribute "ondragstart" "drag(event)"
                ]
                [ div [ class "planner attribute-header" ]
                    [ div [ class "planner attribute-short-wrapper" ]
                        [ h4 [ class "planner attribute-short" ] [ text att.short ] ]
                    , h3 [ class "planner attribute-name", onClick (EditStart key) ] [ text att.name ]
                    , div [ class "planner attribute-desc-wrapper" ]
                        [ span [ class "planner attribute-desc" ] [ description ] ]
                    ]
                , div [ class "planner buttons" ] [ button [ class "planner fas fa-pen", onClick (EditStart key), tabindex 0 ] [] ]
                ]


viewGoals : User -> UserPatchState -> Html StateMsg
viewGoals user userPatchState =
    div []
        [ text "Goals: "
        , user.goals
            |> Array.indexedMap
                (\index goal -> viewGoal index goal userPatchState)
            |> Array.toList
            |> ul []
            |> List.singleton
            |> div []

        -- |> viewTextListToUl
        ]


viewGoal : Int -> Goal -> UserPatchState -> Html StateMsg
viewGoal index goal patchState =
    let
        patching =
            patchState.state == UserLWait

        editing =
            patchState.editing
    in
    -- [ Html.form [ onSubmit <| ViewMessages <| TestMessage index "u" ]
    li []
        [ div []
            [{- Html.form
                [ onSubmit (UpdateGoalPart <| GSend index) ]
                [ input
                    [ value goal.name
                    , onInput (UpdateGoalPart << GName index)
                    , disabled patching
                    , alt "Name"
                    ]
                    []
                , case goal.description of
                    Just desc ->
                        input
                            [ value desc
                            , onInput (UpdateGoalPart << GDescription index)
                            , disabled patching
                            , alt "Description"
                            ]
                            []

                    Nothing ->
                        Html.nothing
                , case editing of
                    Just (GEditState index_ _) ->
                        -- if editstate == AEditState_  then
                        if index_ == index then
                            if patching then
                                text "(...)"

                            else
                                input
                                    [ value "Ok", type_ "submit" ]
                                    []

                        else
                            Html.nothing

                    _ ->
                        Html.nothing
                ]
             -}
            ]
        ]


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    Html.Events.custom "contextmenu"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )


onKeyDown : (Int -> Bool -> msg) -> Html.Attribute msg
onKeyDown msg =
    on "keydown" <|
        D.map2 msg
            keyCode
            (D.field "ctrlKey" D.bool)
