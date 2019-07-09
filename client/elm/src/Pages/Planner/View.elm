module Pages.Planner.View exposing (view, viewAttribute, viewAttributes, viewDay, viewDays, viewGoal, viewGoals, viewResponseHttpError, viewSidebar, viewSyncIndicator, viewTask, viewTasks)

import Array exposing (Array)
import Browser.Navigation as Nav
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
import Svg as Svg
import Svg.Attributes as SvgA



-- implement error message when updating days and receiving error -> spawn/dismiss, stack notification/s
-- implement flask return messages -> missing xy // xy has bad format


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

                    viewUserStatus =
                        viewElementStatus model.user

                    viewDaysStatus =
                        viewElementStatus model.days

                    errorOccurred =
                        case ( model.days, model.user ) of
                            ( _, Just (Err _) ) ->
                                True

                            ( Just (Err _), _ ) ->
                                True

                            _ ->
                                False

                    appendix =
                        if errorOccurred then
                            div [ class "error-buttons-wrapper" ]
                                [ a [ class "reload-button", Route.href Route.App ] [ text "Reload" ]
                                , button [ class "reload-button", onClick <| LoggedinMsg <| LogoutL ] [ text "Log out" ]
                                ]

                        else
                            text ""
                  in
                  div [ class "box" ]
                    [ loading ( "Loading User..." ++ (Tuple.first <| viewUserStatus), Tuple.second <| viewUserStatus )
                    , loading ( "Loading Days..." ++ (Tuple.first <| viewDaysStatus), Tuple.second <| viewDaysStatus )
                    , appendix
                    ]
                ]
            ]

        Loaded model ->
            List.map (Html.map LoadedMsg)
                [ viewOverlay model.partVis
                , viewFixedHeader model.user model.partVis
                , div [ class "planner-wrapper" ]
                    [ viewSidebar model.viewState model.partVis model.user
                    , viewMobileWeekOverview model.today
                    , div [ class "planner-body" ]
                        --, on "scroll" (D.succeed ScrolledDays)
                        [ div [ class "days-top-buffer" ] []
                        , viewDays model.days model.viewState model.today
                        ]
                    ]
                ]


viewFixedHeader : User -> PartVisibility -> Html LoadedMsg
viewFixedHeader user partVis =
    let
        sidebarTglCls =
            if partVis.sidebar then
                "fas fa-arrow-left"

            else
                "fas fa-bars"
    in
    div [ class "fixed-header" ]
        [ div [ class "toggle-sidebar button", onClick ToggleSidebar ]
            [ i [ class sidebarTglCls ] []
            ]
        , h4 [ class "header-text noselect" ] [ text "daycare" ]
        , div [ class "user-menu button", onClick (ShowSettings SettingsOverview) ] [ i [ class "fas fa-user-circle" ] [] ]
        ]


viewMobileWeekOverview today =
    let
        week_start =
            Date.add Date.Days (1 - Date.weekdayNumber today) today

        week_end =
            Date.add Date.Days (7 - Date.weekdayNumber today) today

        range =
            Date.range Date.Day 1 week_start (Date.add Date.Days 1 week_end)
    in
    div [ class "mobile week-overview" ]
        [ div [ class "week-overview-head" ]
            [ button [ class "week-overview-scroll" ] [ i [ class "fas fa-chevron-left" ] [] ]
            , h5 [ class "week-overview-month noselect" ] [ text <| Date.format "MMMM YYYY" (Date.add Date.Days 3 week_start) ]
            , button [ class "week-overview-scroll" ] [ i [ class "fas fa-chevron-right" ] [] ]
            ]
        , div [ class "week-overview-body noselect" ] <| List.map (viewWeekOverviewDay today) range
        ]


viewWeekOverviewDay today date =
    let
        todayClass =
            if date == today then
                " today"

            else
                ""
    in
    div [ class <| "week-overview-day" ++ todayClass ]
        [ span [ class "week-overview-weekday" ]
            [ text <| Date.format "EE" date ]
        , span [ class "week-overview-date" ] [ text <| Date.format "dd" date ]
        ]


viewSidebar : ViewState -> PartVisibility -> User -> Html LoadedMsg
viewSidebar viewState partVis user =
    let
        tglBtnTxt =
            if partVis.sidebar then
                "<"

            else
                ">"

        wrapperClass =
            "sidebar-wrapper"
                ++ (if partVis.sidebar then
                        " show"

                    else
                        " hide"
                   )
    in
    div [ class wrapperClass ]
        [ div [ class "planner sidebar" ]
            [ {- div [ class "planner sidebar-header noselect" ]
                     [ div [ class "planner header" ] [ text user.email ]
                     , div [ class "sidebar-header-icon", onClick (ShowSettings SettingsOverview) ]
                         [ i [ class "fas fa-cog", title "Settings" ] []
                         ]
                     , h2 [ class "planner sidebar-header-text" ] [ text "daycare" ]
                     , viewSyncIndicator viewState.loading
                     ]
                 , hr [ class "planner sidebar" ] []
                 ,
              -}
              div [ class "planner sidebar-body" ]
                [ -- text ("email: " ++ user.email)
                  viewAttributes user viewState
                ]

            -- , button [ class "planner sidebar-expand", onClick ToggleSidebar ] [ text tglBtnTxt ]
            ]
        ]


viewDialog : List (Html LoadedMsg) -> Html LoadedMsg
viewDialog dcont =
    div [ class "fullscreen-overlay darken", onClick <| ShowSettings SettingsHide ]
        [ div [ class "center-dialog", onClick_StopP NoOp ]
            dcont
        ]


viewDialogButton : String -> String -> msg -> Html msg
viewDialogButton icon txt msg =
    button [ class "dialog-button", onClick_StopP msg ]
        (if icon == "" then
            [ span [ class "dialog-button-text" ] [ text txt ] ]

         else
            [ i [ class <| "dialog-button-icon " ++ icon ] []
            , span [ class "dialog-button-text" ] [ text txt ]
            ]
        )


viewOverlay : PartVisibility -> Html LoadedMsg
viewOverlay partVis =
    if partVis.settingsOverlay == Just SettingsOverview then
        viewDialog
            [ viewDialogButton "fas fa-user-cog" "Account Settings" (ShowSettings SettingsAccount)
            , viewDialogButton "fas fa-cog" "Settings" (ShowSettings SettingsSettings)
            , viewDialogButton "fas fa-sign-out-alt" "Logout" Logout
            ]

    else if partVis.settingsOverlay == Just SettingsAccount then
        viewDialog
            [ input [ class "dialog-input", placeholder "Change username" ] []
            , viewDialogButton "arrow-circle-left" "Go back" (ShowSettings SettingsOverview)
            ]

    else if partVis.settingsOverlay == Just SettingsSettings then
        viewDialog
            [ label []
                [ text "Theme"
                , select [ class "dialog-selector" ]
                    [ option [] [ text "light" ]
                    , option [] [ text "dark" ]
                    ]
                ]
            , viewDialogButton "arrow-circle-left" "Go back" (ShowSettings SettingsOverview)
            ]

    else
        div [ class "fullscreen-overlay darken hidden" ] []


viewSyncIndicator : Maybe ViewLoadState -> Html LoadedMsg
viewSyncIndicator loading =
    div [ class "sidebar-header-icon", style "margin-left" "auto" ]
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
                    [ class "fas fa-check-circle"
                    , title "Up to date!"
                    ]
                    []
        ]


viewDays : Array Day -> ViewState -> Date -> Html LoadedMsg
viewDays days viewState today =
    div [ class "days" ] <|
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


viewDay : ViewState -> Int -> Day -> Html LoadedMsg
viewDay viewState index day =
    -- case day_.day of
    --     Just day ->
    let
        ( textDesc, descClassApp, emptyDesc ) =
            case day.description of
                Just "" ->
                    ( "Click to add description.", " add", "" )

                Nothing ->
                    ( "Click to add description.", " add", "" )

                Just other ->
                    ( other, "", other )

        viewDefaultDesc =
            p [ class <| "day-description" ++ descClassApp ] [ text textDesc ]

        viewDescription =
            div [ class "day-description-wrapper", onClick (EditStart <| DayPartKey index <| DDescription emptyDesc) ]
                [ case viewState.editing of
                    Just (DayPartKey index_ (DDescription desc)) ->
                        if index == index_ then
                            Html.form [ onSubmit EditCommit ]
                                [ input [ id "edit-input", class "day-description", onKeyDown KeyDown, onInput (EditUpdate << DayPartKeyW index << Just << DDescription), value desc ] []
                                ]

                        else
                            viewDefaultDesc

                    _ ->
                        viewDefaultDesc
                ]

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

        focusing =
            Tuple.first viewState.dayInFocus == index
    in
    div [ class "day", id <| "day_" ++ String.fromInt index ]
        [ div [ class "day-date-bar noselect" ]
            [ div [ class "day-date-bar-accent" ] []
            , div [ class "day-date-bar-wrapper" ]
                [ h4 [ class "day-date" ] [ text <| Date.format "dd" day.date ]
                , span [ class "day-date-weekday" ] [ text <| Date.format "EE" day.date ]

                -- , span [ class "day-description" ] [
                , viewDescription

                -- , if focusing then
                --     span [] [ text "in focus" ]
                --   else
                --     text ""
                ]
            ]
        , div [ class "day-body" ]
            [ viewTasks index day adding editing ]

        -- , viewTimeline day
        ]



{- Nothing ->
   let
       date =
           Date.format "dd.MM.YYYY" day_.date

       weekday =
           Date.format "EEEE" day_.date
   in
   div [ class "day nocontent", onClick (AddDay index day_.date) ]
       [ div [ class "day-header" ]
           [ h4 [ class "day-date weekday noselect" ] [ text <| weekday ++ ", " ]
           , h4 [ class "day-date noselect" ] [ text date ]
           ]
       , text "click to add day"
       ]
-}


viewTasks : Int -> Day -> Bool -> Maybe ContentKey -> Html LoadedMsg
viewTasks index day adding editing =
    let
        viewAddTask =
            div [ class "task-body add-task" ]
                [ div [ class "task-editing-icon" ] [ i [ class "fas fa-plus" ] [] ]
                ]

        viewEmptyTasks =
            div [ class "task-body empty" ]
                [ span [ class "day-tasks-empty-text" ] [ text "No Tasks - Try adding one now!" ] ]
    in
    ul [ class "day-tasks noselect" ] <|
        (if Array.length day.tasks == 0 then
            [ viewEmptyTasks ]

         else
            Array.indexedMap (viewTask index editing) day.tasks
                |> Array.toList
        )
            ++ [ viewAddTask ]



{- ((Array.indexedMap (viewTask index editing) day.tasks
       |> Array.toList
    )
       -- ++ [ div [ class "planner day-task add" ]
       ++ [ if adding then
               li [ class "day-task adding" ]
                   [ Html.form [ class "add-task-form", onSubmit AddCommit ]
                       [ input [ id "edit-input", class "add-task-input", onInput AddUpdate, placeholder "task", autofocus True, autocomplete False ] [] ]
                   ]

            else
               li [ class "day-task add-task", onClick (AddStart <| DayTaskKeyW index 0 Nothing) ]
                   [ i [ class "fas fa-plus" ] []
                   ]
          ]
   )
-}
-- |> List.reverse
--    ]


viewTimeline : Day -> Html LoadedMsg
viewTimeline day =
    let
        day_start =
            8.0

        day_end =
            19.5

        width =
            300

        height =
            48

        viewTaskBox : Float -> Float -> String -> Html LoadedMsg
        viewTaskBox start end name =
            let
                twidth =
                    (end - start) / (day_end - day_start) * width

                tx =
                    (start - day_start) / (day_end - day_start) * width
            in
            Svg.g
                []
                [ Svg.rect
                    [ SvgA.width (String.fromFloat <| twidth)
                    , SvgA.height (String.fromInt height)
                    , SvgA.x (String.fromFloat <| tx)
                    , SvgA.fill "lightblue"
                    , SvgA.stroke "grey"
                    , SvgA.strokeWidth "1"
                    ]
                    []
                , Svg.text_
                    [ SvgA.x (String.fromFloat <| tx + (twidth / 2))
                    , SvgA.y (String.fromFloat <| height / 2)
                    , SvgA.textAnchor "middle"
                    , SvgA.fill "black"
                    ]
                    [ Svg.text name ]
                ]

        viewTaskBoxMaybe : Task -> Html LoadedMsg
        viewTaskBoxMaybe task =
            case task.time of
                Nothing ->
                    Svg.g [] []

                Just time ->
                    viewTaskBox time.start time.end task.name

        _ =
            Debug.log "tasks" day.tasks
    in
    Svg.svg
        [ SvgA.width (String.fromInt <| width + 5)
        , SvgA.height (String.fromInt <| height + 2)
        , SvgA.viewBox <| "0 0 " ++ (String.fromInt <| width + 5) ++ " " ++ (String.fromInt <| height + 2)
        ]
        [ Svg.g
            []
            (Array.map viewTaskBoxMaybe (Array.filter (\t -> t.time /= Nothing) day.tasks)
                |> Array.toList
            )
        , Svg.rect
            [ SvgA.width (String.fromInt width)
            , SvgA.height (String.fromInt height)
            , SvgA.x "1"
            , SvgA.y "1"
            , SvgA.rx "4"
            , SvgA.strokeWidth "1.5"
            , SvgA.stroke "black"
            , SvgA.fill "transparent"
            ]
            []
        ]


viewScheduledTask : Int -> Maybe ContentKey -> Int -> Task -> ( Float, Float ) -> Html LoadedMsg
viewScheduledTask dayindex maybeediting taskindex task timeTuple =
    div [ class "task-wrapper" ]
        [ div [ class "task-time-wrapper" ]
            [ span [ class "task-time time-start" ] [ text <| String.fromFloat <| Tuple.first timeTuple ]
            , span [ class "task-time time-end" ] [ text <| String.fromFloat <| Tuple.second timeTuple ]
            ]
        , viewTask dayindex maybeediting taskindex task
        ]


viewTask : Int -> Maybe ContentKey -> Int -> Task -> Html LoadedMsg
viewTask dayindex maybeediting taskindex task =
    let
        ( classCompleted, classCheck ) =
            if task.state == TSCompleted then
                ( "task-done", "times" )

            else
                ( "task-todo", "check" )

        ( classImportant, classStar ) =
            if task.important then
                ( " orange", "fas fa-star" )

            else
                ( "", "far fa-star" )

        editingThis =
            case maybeediting of
                Just (DayPartKey dayindex_ (DTask taskindex_ task_)) ->
                    (dayindex == dayindex_) && (taskindex == taskindex_)

                _ ->
                    False

        ( classEditing, classChevron ) =
            if editingThis then
                ( " editing", "fas fa-chevron-right" )

            else
                ( "", "fas fa-chevron-left" )
    in
    div [ class <| "task-body " ++ classCompleted ++ classEditing ++ classImportant ]
        [ div [ class "task-color-accent" ] []
        , h4 [ class "task-title noselect" ] [ text task.name ]
        , div [ class <| "task-editing-wrapper" ++ classEditing ]
            [ if editingThis then
                div [ class "task-toggle-expand hover-button", onClick <| EditDiscard ] [ i [ class classChevron ] [] ]

              else
                div [ class "task-toggle-expand hover-button", onClick <| EditStart <| DayPartKey dayindex <| DTask taskindex task ] [ i [ class classChevron ] [] ]
            , div [ class "task-editing-icons" ]
                [ div [ class "task-editing-icon hover-button", onClick <| ToggleDayTask DTImportant dayindex task taskindex ]
                    [ i [ class classStar ] [] ]
                , div [ class "task-editing-icon hover-button" ]
                    [ i [ class "far fa-bell" ] [] ]

                -- , div [ class "task-editing-icon hover-button" ]
                --     [ i [ class "far fa-alarm-plus" ] [ viewBellIcon ] ]
                , div [ class "task-editing-icon hover-button" ]
                    [ i [ class "far fa-clock" ] [] ]
                , div [ class "task-editing-icon hover-button", onClick <| ToggleDayTask DTState dayindex task taskindex ]
                    [ i [ class <| "fas fa-" ++ classCheck ] [] ]
                ]
            ]
        ]


viewTask_ : Int -> Maybe ContentKey -> Int -> Task -> Html LoadedMsg
viewTask_ dayindex maybeediting taskindex task =
    let
        classCompleted =
            if task.state == TSCompleted then
                "task-done"

            else
                "task-todo"

        defaultView =
            li
                [ class <| "planner day-task noselect " ++ classCompleted
                , onClick (ToggleDayTask DTState dayindex task taskindex)
                , onRightClick (EditStart <| DayPartKey dayindex <| DTask taskindex task)
                ]
                [ text task.name ]
    in
    case maybeediting of
        Just (DayPartKey index_ (DTask taskindex_ task_)) ->
            if taskindex == taskindex_ then
                let
                    tasknameinput =
                        input
                            [ id "edit-input"
                            , class "day-task editing-input"

                            -- , style "width" <| (String.fromInt <| String.length task_.name) ++ ".5ch"
                            , value task_.name
                            , autofocus True
                            , maxlength 30
                            , minlength 3
                            , autocomplete False
                            , attribute "data-lpignore" "true"
                            , onInput (\s -> EditUpdate <| DayTaskKeyW dayindex taskindex <| Just <| DTName s)
                            , onKeyDown KeyDown
                            , type_ "text"
                            ]
                            []
                in
                [ Html.form [ onSubmit EditCommit, onKeyDown KeyDown ] [ tasknameinput ] ]
                    |> viewDialog
                {- li [ class <| "day-task editing" ]
                   [
                       [
                       ]
                   ]
                -}

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


viewBellIcon : Html LoadedMsg
viewBellIcon =
    Svg.svg
        [ SvgA.viewBox "0 0 16 16"
        , SvgA.width "16"
        , SvgA.height "16"
        ]
        [ Svg.g
            [ SvgA.transform "scale(0.03125)"
            ]
            [ Svg.path
                [ SvgA.fill "currentColor"
                , SvgA.scale "0.03125"
                , SvgA.d "M256 64C132.3 64 32 164.29 32 288a222.7 222.7 0 0 0 44.79 134l-40.1 40.09a16 16 0 0 0 0 22.63l22.62 22.62a16 16 0 0 0 22.63 0L122 467.22a222.82 222.82 0 0 0 268 0l40.1 40.09a16 16 0 0 0 22.62 0l22.63-22.62a16 16 0 0 0 0-22.63L435.25 422A222.69 222.69 0 0 0 480 288c0-123.71-100.26-224-224-224zm0 400a176 176 0 1 1 176-176 176 176 0 0 1-176 176zm96-200h-72v-72a16 16 0 0 0-16-16h-16a16 16 0 0 0-16 16v72h-72a16 16 0 0 0-16 16v16a16 16 0 0 0 16 16h72v72a16 16 0 0 0 16 16h16a16 16 0 0 0 16-16v-72h72a16 16 0 0 0 16-16v-16a16 16 0 0 0-16-16zM96 0A96 96 0 0 0 0 96a94.81 94.81 0 0 0 15.3 51.26L161.2 25.68A95.63 95.63 0 0 0 96 0zm320 0a95.66 95.66 0 0 0-65.18 25.66l145.89 121.57A94.85 94.85 0 0 0 512 96a96 96 0 0 0-96-96z"
                ]
                []
            ]
        ]


onClick_StopP : msg -> Html.Attribute msg
onClick_StopP msg =
    Html.Events.custom "click"
        (D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )



-- onDragStart : msg -> Html.Attribute msg
-- onDragStart msg =
--     Html.Events.custom "ondragstart"


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
