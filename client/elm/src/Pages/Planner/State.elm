port module Pages.Planner.State exposing (ctrlEnterPort, editingPort, getToday, init, loadToken, loadedInit, loadedTokenPlanner, loggedinInit, saveTokenPlanner, stringFromDateTuple, subscriptions, toSession, update, updateLoaded)

import Array exposing (Array)
import Browser.Dom
import Date exposing (Date)
import Dialog exposing (Dialog)
import Http
import Json.Decode as D
import Json.Encode as E
import Pages.Planner.Rest as Rest exposing (..)
import Pages.Planner.Types as Model exposing (..)
import Route
import Session exposing (Session)
import Task
import Time exposing (Month(..))


init : Session -> ( StateModel, Cmd StateMsg )
init session =
    let
        range =
            ( Date.fromCalendarDate 2001 Jul 25, Date.fromCalendarDate 2001 Jul 25 )

        lModel token_ =
            Loggedin
                { session = session
                , token = token_
                , user = Nothing
                , days = Nothing
                , today = Tuple.first range
                , dayRange = range
                }
    in
    case Session.token session of
        Just token ->
            let
                _ =
                    Debug.log "loaded token via session" token
            in
            loggedinInit session token (Tuple.first range)

        _ ->
            ( Guest (GuestModel session (Tuple.first range)), loadToken () )


loggedinInit : Session -> String -> Date -> ( StateModel, Cmd StateMsg )
loggedinInit session token today =
    let
        range : ( Date, Date )
        range =
            ( today, Date.add Date.Days -7 today )

        model : LoggedinModel
        model =
            { session = session
            , token = token
            , user = Nothing
            , days = Nothing
            , dayRange = range
            , today = today
            }
    in
    ( Loggedin model, Cmd.batch [ saveTokenPlanner token, getToday, loadUser (LoggedinMsg << LoadedUser) token ] )



--, loadDays token range


loadedInit : Session -> String -> User -> Array RangeDay -> Date -> ( StateModel, Cmd StateMsg )
loadedInit session token user days today =
    let
        range =
            ( today, Date.add Date.Days -7 today )
    in
    ( Loaded
        { session = session
        , token = token
        , user = user
        , days = days
        , viewState =
            { editing = Nothing

            --  hovering = Nothing
            , loading = Nothing
            , adding = Nothing
            , dayRange = range
            }
        , partVis =
            { sidebar = False
            , settingsOverlay = Nothing
            }
        , today = today

        -- , dialog = Nothing
        }
    , Cmd.none
    )



-- Update


update : StateMsg -> StateModel -> ( StateModel, Cmd StateMsg )
update msg_ model_ =
    case ( model_, msg_ ) of
        ( Guest model, GuestMsg msg ) ->
            case msg of
                LoadedToken maybetoken ->
                    case maybetoken of
                        "" ->
                            ( model_, Cmd.none )

                        token ->
                            loggedinInit model.session token model.today

                _ ->
                    ( Guest model, Cmd.none )

        ( Loggedin model, LoggedinMsg msg ) ->
            case msg of
                LoadedDays (Ok days) ->
                    case model.user of
                        Just (Ok user) ->
                            loadedInit model.session model.token user days model.today

                        _ ->
                            ( Loggedin { model | days = Just <| Ok days }, Cmd.none )

                LoadedDays (Err err) ->
                    ( Loggedin { model | days = Just <| Err err }, Cmd.none )

                LoadedUser (Ok user) ->
                    case model.days of
                        Just (Ok days) ->
                            loadedInit model.session model.token user days model.today

                        _ ->
                            ( Loggedin { model | user = Just (Ok user) }, Cmd.none )

                LoadedUser (Err err) ->
                    ( Loggedin { model | user = Just <| Err err }, Cmd.none )

                Today date ->
                    let
                        range =
                            ( date, Date.add Date.Days -7 date )
                    in
                    -- update (LoadDays range) model
                    ( Loggedin { model | today = date, dayRange = range }, loadDays (LoggedinMsg << LoadedDays) model.token range )

                LogoutL ->
                    ( model_, Cmd.none )

        -- LoadedToken token ->
        --     let
        --         key =
        --             Session.navKey model.session
        --     in
        --     if token /= "" then
        --         ( Loggedin { model | session = Session.fromString key (Just token) }
        --         , Cmd.batch [ saveTokenPlanner token, Route.replaceUrl key Route.App ]
        --         )
        --     else
        --         ( Loggedin model, Cmd.none )
        ( Loaded model, LoadedMsg msg ) ->
            updateLoaded msg model
                |> (\r -> ( Loaded <| Tuple.first r, Tuple.second r ))

        ( _, _ ) ->
            ( model_, Cmd.none )


updateLoaded : LoadedMsg -> LoadedModel -> ( LoadedModel, Cmd StateMsg )
updateLoaded msg model =
    let
        cmdnone m =
            ( m, Cmd.none )
    in
    case msg of
        UpdatedUser (Ok user) ->
            cmdnone { model | user = user }

        UpdatedDays (Ok days) ->
            cmdnone { model | days = days, viewState = (\vs -> { vs | loading = Nothing }) model.viewState }

        EditDiscard ->
            cmdnone
                { model
                    | viewState =
                        (\vs -> { vs | editing = Nothing, loading = Nothing }) model.viewState
                }

        EditRemove key ->
            Debug.todo "not implemented clickremove"

        EditUpdate element ->
            case element of
                AttributeKeyW (Just part) ->
                    case model.viewState.editing of
                        Just (AttributeKey index att) ->
                            let
                                newAtt =
                                    case part of
                                        AShort value ->
                                            { att | short = value }

                                        AName value ->
                                            { att | name = value }

                                        ADescription value ->
                                            { att | description = Just value }
                            in
                            cmdnone
                                { model
                                    | viewState =
                                        (\vs ->
                                            { vs | editing = Just (AttributeKey index newAtt) }
                                        )
                                            model.viewState
                                }

                        _ ->
                            cmdnone model

                DayTaskKeyW dindex tindex (Just (DTName string)) ->
                    case model.viewState.editing of
                        Just (DayPartKey _ (DTask _ task)) ->
                            let
                                newtask =
                                    { task | name = string }
                            in
                            cmdnone { model | viewState = (\vs -> { vs | editing = Just <| DayPartKey dindex <| DTask tindex newtask }) model.viewState }

                        _ ->
                            cmdnone model

                DayPartKeyW dayindex (Just (DDescription desc)) ->
                    case model.viewState.editing of
                        Just (DayPartKey _ (DDescription _)) ->
                            cmdnone { model | viewState = (\vs -> { vs | editing = Just <| DayPartKey dayindex (DDescription desc) }) model.viewState }

                        _ ->
                            cmdnone model

                _ ->
                    Debug.todo "implement other: editupdate"

        EditStart element ->
            let
                vsNone =
                    { editing = Nothing, hovering = Nothing, loading = Nothing }

                updateViewState editing =
                    (\vs -> { vs | editing = editing }) model.viewState

                -- if expanded then
                -- save to model, retract and expand other (key)
                vsNothing =
                    updateViewState Nothing

                vsNothingLoad =
                    { vsNothing | loading = Just ViewLLoading }
            in
            case element of
                AttributeKey index att ->
                    let
                        key =
                            AttributeKey index att

                        vsKey =
                            updateViewState (Just key)

                        vsKeyLoad =
                            { vsKey | loading = Just ViewLLoading }

                        action =
                            case att.name_ of
                                "" ->
                                    postAttribute model.token

                                _ ->
                                    patchAttribute model.token
                    in
                    case model.viewState.editing of
                        Just (AttributeKey index_ att_) ->
                            let
                                modelNewUser =
                                    { model | user = (\u -> { u | attributes = Array.set index_ att_ model.user.attributes }) model.user }
                            in
                            if att == att_ then
                                {- dont update but retract -}
                                cmdnone { model | viewState = vsNothing }
                                -- else if index == index_ then -- index == index_ &&
                                --     {- update and retract -}
                                --     ( { model | user = UserLSuccess userNewAttributes, viewState = vsNothingLoad }, action att_ )

                            else
                                {- update and expand -}
                                ( { modelNewUser | viewState = vsKeyLoad }, Cmd.batch [ action att_, editingPort (), focusEdit ] )

                        Just (GoalKey index_ goal_) ->
                            let
                                modelNewUser =
                                    { model | user = (\u -> { u | goals = Array.set index_ goal_ model.user.goals }) model.user }
                            in
                            {- update goal and expand -}
                            ( { modelNewUser | viewState = vsKeyLoad }, Cmd.batch [ patchGoals model.token goal_, editingPort (), focusEdit ] )

                        _ ->
                            cmdnone model

                -- DayTaskKey dayindex task taskindex ->
                --     ( { model | viewState = (\vs -> { vs | editing = Just element }) model.viewState }, focusEdit )
                DayPartKey dayindex daypart ->
                    ( { model | viewState = (\vs -> { vs | editing = Just element }) model.viewState }, focusEdit )

                {- case daypart of
                   DDescription desc ->
                       case getDayFromArray dayindex model.days of
                           Just day ->
                               let
                                   newday =
                                       { day | description = Just desc }

                                   newrangeday =
                                       { date = day.date
                                       , day = Just newday
                                       }

                                   newdays =
                                       Array.set dayindex newrangeday model.days
                               in
                               ( { model | days = newdays }, patchDay model.token newday )

                           Nothing ->
                               cmdnone model

                   _ ->
                       Debug.todo "implement other: editstart daypartkey"
                -}
                _ ->
                    Debug.todo "implement other: editstart"

        EditCommit ->
            let
                modelNoEditing =
                    { model | viewState = (\vs_ -> { vs_ | editing = Nothing }) model.viewState }
            in
            case model.viewState.editing of
                Just (AttributeKey index att) ->
                    ( { model | user = (\u -> { u | attributes = Array.set index att model.user.attributes }) model.user }
                    , patchAttribute model.token att
                    )

                -- Just (GoalKey index goal) ->
                --     ( modelNoEditing, Cmd.none )
                Just (DayPartKey dayindex (DTask taskindex task)) ->
                    case Array.get dayindex model.days of
                        Just rangeday ->
                            case rangeday.day of
                                Just day_ ->
                                    let
                                        tasks =
                                            case task.name of
                                                "" ->
                                                    Array.append
                                                        (Array.slice 0 taskindex day_.tasks)
                                                        (Array.slice (taskindex + 1) (1 + Array.length day_.tasks) day_.tasks)

                                                _ ->
                                                    Array.set taskindex task day_.tasks

                                        day =
                                            { day_ | tasks = tasks }

                                        days =
                                            Array.set dayindex (RangeDay day.date (Just day)) model.days
                                    in
                                    ( { modelNoEditing | days = days }, patchDay model.token day )

                                Nothing ->
                                    cmdnone modelNoEditing

                        _ ->
                            cmdnone modelNoEditing

                Just (DayPartKey dayindex daypart) ->
                    case daypart of
                        DDescription desc ->
                            case getDayFromArray dayindex model.days of
                                Just day ->
                                    let
                                        newday =
                                            { day | description = Just desc }

                                        newrangeday =
                                            { date = day.date
                                            , day = Just newday
                                            }

                                        newdays =
                                            Array.set dayindex newrangeday model.days
                                    in
                                    ( { modelNoEditing | days = newdays }, patchDay model.token newday )

                                Nothing ->
                                    cmdnone modelNoEditing

                        _ ->
                            Debug.todo "implement other: editstart daypartkey"

                Nothing ->
                    cmdnone modelNoEditing

                _ ->
                    Debug.todo "editcommit implement other"

        Synchronize (Ok _) ->
            ( { model | viewState = (\vs -> { vs | loading = Just ViewLSuccess }) model.viewState }, Cmd.batch [ loadUser (LoadedMsg << UpdatedUser) model.token, loadDays (LoadedMsg << UpdatedDays) model.token model.viewState.dayRange ] )

        Synchronize (Err err) ->
            ( { model | viewState = (\vs -> { vs | loading = Just (ViewLError err) }) model.viewState }, Cmd.batch [ loadUser (LoadedMsg << UpdatedUser) model.token, loadDays (LoadedMsg << UpdatedDays) model.token model.viewState.dayRange ] )

        AddStart element ->
            case element of
                AttributeKeyW _ ->
                    let
                        att =
                            Model.Attribute "" "" "" Nothing

                        index =
                            Array.length model.user.attributes - 1
                    in
                    ( { model | viewState = (\vs -> { vs | adding = Just <| AttributeKey index att }) model.viewState }, focusEdit )

                DayTaskKeyW dindex tindex Nothing ->
                    let
                        task =
                            Task "" "" TSTodo Nothing

                        -- Task "" "" TSTodo Nothing
                    in
                    ( { model | viewState = (\vs -> { vs | adding = Just <| DayPartKey dindex <| DTask 0 task }) model.viewState }, focusEdit )

                _ ->
                    Debug.todo "addelementshow for other elements"

        AddUpdate value ->
            case model.viewState.adding of
                Just (DayPartKey dindex (DTask _ _)) ->
                    let
                        task =
                            Task value value TSTodo Nothing
                    in
                    cmdnone { model | viewState = (\vs -> { vs | adding = Just <| DayPartKey dindex <| DTask 0 task }) model.viewState }

                _ ->
                    Debug.todo "implement other edit"

        AddDiscard ->
            cmdnone { model | viewState = (\vs -> { vs | adding = Nothing }) model.viewState }

        AddCommit ->
            let
                ( model__, msg__ ) =
                    case model.viewState.adding of
                        Just (DayPartKey dayindex (DTask tindex task)) ->
                            let
                                maybeday =
                                    Array.get dayindex model.days

                                -- days_ =
                            in
                            case maybeday of
                                Just rangeday ->
                                    case rangeday.day of
                                        Just day_ ->
                                            let
                                                tasks =
                                                    case task.name of
                                                        "" ->
                                                            day_.tasks

                                                        _ ->
                                                            Array.append day_.tasks <| Array.fromList [ task ]

                                                day =
                                                    { day_ | tasks = tasks }

                                                days =
                                                    Array.set dayindex (RangeDay day.date (Just day)) model.days
                                            in
                                            ( { model | days = days }, patchDay model.token day )

                                        Nothing ->
                                            ( model, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        _ ->
                            Debug.todo "implement other submit"
            in
            ( { model__ | viewState = (\vs -> { vs | adding = Nothing }) model.viewState }, msg__ )

        ToggleSidebar ->
            cmdnone { model | partVis = (\pv -> { pv | sidebar = not pv.sidebar }) model.partVis }

        AddDay index date ->
            let
                newday =
                    Day date date model.user.email Nothing [] (Array.fromList [])

                rangeday =
                    RangeDay date (Just newday)

                days =
                    Array.set index rangeday model.days
            in
            updateLoaded (SynchronizeLoad (postDay model.token newday)) { model | days = days }

        ToggleDayTask dayindex task taskindex ->
            let
                maybeday =
                    Array.get dayindex model.days

                badresponse =
                    cmdnone { model | viewState = (\vs -> { vs | loading = Just <| ViewLError <| Http.BadStatus 418 }) model.viewState }
            in
            case maybeday of
                Just maybeday_ ->
                    case maybeday_.day of
                        Just day ->
                            let
                                newtask =
                                    if task.state == TSCompleted then
                                        { task | state = TSTodo }

                                    else
                                        { task | state = TSCompleted }

                                newtasks =
                                    Array.set taskindex newtask day.tasks

                                newday =
                                    { day | tasks = newtasks }

                                newdays =
                                    Array.set dayindex { maybeday_ | day = Just newday } model.days
                            in
                            updateLoaded (SynchronizeLoad (patchDay model.token newday)) { model | days = newdays }

                        Nothing ->
                            badresponse

                Nothing ->
                    badresponse

        SynchronizeLoad cmdmsg ->
            ( { model | viewState = (\vs -> { vs | loading = Just ViewLLoading }) model.viewState }, cmdmsg )

        KeyDown code ctrlKey ->
            if code == 27 then
                case model.viewState.editing of
                    Nothing ->
                        case model.viewState.adding of
                            Nothing ->
                                cmdnone model

                            _ ->
                                updateLoaded AddDiscard model

                    _ ->
                        updateLoaded EditDiscard model

            else if (code == 13 || code == 10) && ctrlKey then
                case model.viewState.adding of
                    Nothing ->
                        cmdnone model

                    Just (DayPartKey dayindex (DTask tindex task)) ->
                        let
                            commitUpdate =
                                updateLoaded AddCommit model

                            addNewUpdate =
                                updateLoaded (AddStart <| DayTaskKeyW 0 dayindex Nothing) (Tuple.first commitUpdate)
                        in
                        ( Tuple.first addNewUpdate, Cmd.batch [ Tuple.second addNewUpdate, Tuple.second commitUpdate ] )

                    _ ->
                        cmdnone model
                -- _ ->
                --     Debug.todo "implement other ctrl+enter -> save and add next"

            else
                cmdnone model

        LoadMoreDays ->
            let
                newRange =
                    ( model.today, Date.add Date.Days -7 <| Tuple.second model.viewState.dayRange )
                        |> Debug.log "newrange"
            in
            ( { model | viewState = (\vs -> { vs | loading = Just ViewLLoading, dayRange = newRange }) model.viewState }, loadDays (LoadedMsg << UpdatedDays) model.token model.viewState.dayRange )

        HideSettings ->
            cmdnone { model | partVis = (\pv -> { pv | settingsOverlay = Nothing }) model.partVis }

        ShowSettings part ->
            cmdnone { model | partVis = (\pv -> { pv | settingsOverlay = Just part }) model.partVis }

        Logout ->
            ( model, Cmd.batch [ logoutPort (), Route.replaceUrl (Session.navKey model.session) Route.Home ] )

        _ ->
            cmdnone model


stringFromDateTuple : ( Date, Date ) -> String
stringFromDateTuple range =
    let
        t =
            Tuple.mapBoth (Date.format "ddMMYYYY") (Date.format "ddMMYYYY") range
    in
    Tuple.first t ++ ", " ++ Tuple.second t


toSession : StateModel -> Session
toSession model =
    case model of
        Guest guest ->
            guest.session

        Loggedin loggedin ->
            loggedin.session

        Loaded loaded ->
            loaded.session


focusEdit : Cmd StateMsg
focusEdit =
    Task.attempt (\_ -> LoadedMsg <| NoOp) (Browser.Dom.focus "edit-input")


getToday : Cmd StateMsg
getToday =
    Task.perform (LoggedinMsg << Today) Date.today


getDayFromArray index days =
    let
        maybeday_ =
            Array.get index days
    in
    case maybeday_ of
        Just maybeday ->
            case maybeday.day of
                Just day ->
                    Just day

                _ ->
                    Nothing

        _ ->
            Nothing



-- Json


port ctrlEnterPort : (() -> msg) -> Sub msg


port editingPort : () -> Cmd msg


port loadedTokenPlanner : (String -> msg) -> Sub msg


port saveTokenPlanner : String -> Cmd msg


port loadToken : () -> Cmd msg


port logoutPort : () -> Cmd msg


subscriptions : StateModel -> Sub StateMsg
subscriptions model =
    -- let
    --     msg =
    --         case model of
    --             Guest _ ->
    --                 GuestMsg << LoadedTokenGuest
    --             _ ->
    --                 LoggedinMsg << LoadedToken
    -- in
    Sub.batch
        [ ctrlEnterPort (\_ -> LoadedMsg <| EditCommit)
        , loadedTokenPlanner (GuestMsg << LoadedToken)
        ]
