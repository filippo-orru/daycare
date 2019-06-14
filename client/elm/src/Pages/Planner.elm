port module Pages.Planner exposing (Attribute, AttributeKey(..), ContentKey(..), ContentKeyW(..), DayKey(..), DaysLoadState(..), EditState(..), Goal, GoalKey(..), GuestModel, GuestMsg(..), LoadedModel, LoadedMsg(..), LoggedinModel, LoggedinMsg(..), Model, StateModel(..), StateMsg(..), User, UserLevel(..), UserLoadState(..), UserPart(..), UserPatchState, ViewLoadState(..), ViewState, addAttribute, decodeAttribute, decodeAttributeKey, decodeGoal, decodeGoalKey, decodeLevel, decodeUser, editingPort, encodeAttribute, encodeGoal, getToday, init, loadDays, loadUser, loadedInit, loadedTokenPlanner, loggedinInit, patchAttribute, patchDay, patchGoals, saveTokenPlanner, submitEditPort, subscriptions, toSession, update, updateLoaded, view, viewAttribute, viewAttributes, viewDay, viewDays, viewGoal, viewGoals, viewResponseHttpError, viewSidebar, viewSyncIndicator, viewTask, viewTasks)

import Array exposing (Array)
import Date exposing (Date)
import Days exposing (Day, Days, Task)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver, onSubmit)
import Html.Extra as Html
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4)
import Http
import Json.Decode as D
import Json.Encode as E
import Route
import Session exposing (Session)
import Task
import Time exposing (Month(..))


type alias Model =
    { session : Session
    , user : UserLoadState
    , days : DaysLoadState
    , viewState : ViewState
    , errormsg : Maybe String
    , today : Date
    }


type StateModel
    = Guest GuestModel
    | Loggedin LoggedinModel
    | Loaded LoadedModel


type alias GuestModel =
    { session : Session
    , today : Date
    }


type alias LoggedinModel =
    { session : Session
    , token : String
    , user : Maybe (Result Http.Error User)
    , days : Maybe (Result Http.Error (Array Day))
    , dayRange : ( Date, Date )
    , today : Date
    }


type alias LoadedModel =
    { session : Session
    , token : String
    , user : User
    , days : Array Day
    , viewState : ViewState
    , today : Date
    }



-- , userPatchState : UserPatchState
-- , days : Days.Model


type alias ViewState =
    { loading : Maybe ViewLoadState

    -- , hovering : Maybe ContentKey
    , editing : Maybe ContentKey
    , sidebarExpanded : Bool
    , adding : Maybe ContentKey
    , dayRange : ( Date, Date )
    }


type UserLoadState
    = UserLIdle
    | UserLWait
    | UserLError Http.Error
    | UserLSuccess User


type DaysLoadState
    = DaysLIdle
    | DaysLWait
    | DaysLError Http.Error
    | DaysLSuccess (Array Day)


type alias UserPatchState =
    { state : UserLoadState
    , editing : Maybe EditState
    }


type ViewLoadState
    = ViewLLoading
    | ViewLError Http.Error
    | ViewLSuccess


type alias User =
    { email : String
    , attributes : Array Attribute
    , goals : Array Goal
    , level : UserLevel
    }


type alias Attribute =
    { name_ : String
    , name : String
    , short : String
    , description : Maybe String
    }


type alias Goal =
    { name_ : String
    , name : String
    , description : Maybe String
    , deadline : Maybe String
    }


type UserLevel
    = LevelUser
    | LevelMod
    | LevelAdmin


type EditState
    = GEditState Int Goal
    | AEditState Int Attribute
    | DEditState Day
    | DTaskEditState Date Task


type UserPart
    = LoadedAttribute (Result Http.Error Attribute)
    | LoadedGoal (Result Http.Error Goal)



-- type PartKey


type AttributeKey
    = AShort String
    | AName String
    | ADescription String



-- | ASend Int


type GoalKey
    = GName Int
    | GDescription Int
    | GSend Int


type DayKey
    = DDate Int
    | DDescription Int


type ContentKey
    = AttributeKey Int Attribute
    | GoalKey Int Goal
    | DayTaskKey Int Days.Task
    | DayKey Int Days.Day


type ContentKeyW
    = AttributeKeyW (Maybe AttributeKey)
    | GoalkeyW (Maybe GoalKey)
    | DayTaskKeyW Int
    | DayKeyW (Maybe DayKey)


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
                model =
                    lModel token
            in
            ( model, Cmd.batch [ getToday model, loadUser token, loadDays token range ] )

        _ ->
            ( Guest { session = session, today = Tuple.first range }, Cmd.none )


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
    ( Loggedin model, Cmd.batch [ getToday (Loggedin model), loadUser token, loadDays token range ] )


loadedInit : Session -> String -> User -> Array Day -> Date -> ( StateModel, Cmd StateMsg )
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
            , sidebarExpanded = False
            , adding = Nothing
            , dayRange = range
            }
        , today = today
        }
    , Cmd.none
    )



{- let
       range =
           ( Date.fromCalendarDate 2001 Jul 25, Date.fromCalendarDate 2001 Jul 25 )

       model =
           { session = session
           , user = UserLIdle
           , days = DaysLIdle

           -- , userPatchState =
           --     { state = UserLIdle
           --     , editing = Nothing
           --     }
           , viewState =
               { editing = Nothing

               --  hovering = Nothing
               , loading = Nothing
               , sidebarExpanded = False
               , adding = Nothing
               , dayRange = range
               }

           -- , dayLoadState = Days.LoadingStateIdle
           , errormsg = Nothing
           , today = Date.fromCalendarDate 2001 Jul 25
           }

       maybetoken =
           Session.token session

       -- range =
       --     ( Date.fromCalendarDate 2019 Jun 6, Date.fromCalendarDate 2019 Jun 4 )
   in
   case maybetoken of
       Nothing ->
           ( model, Cmd.none )

       Just token ->
           ( { model | user = UserLWait }
             -- , days = DaysLWait }
           , Cmd.batch [ loadUser token, getToday ]
           )
-}
-- Update


type StateMsg
    = GuestMsg GuestMsg
    | LoggedinMsg LoggedinMsg
    | LoadedMsg LoadedMsg


type GuestMsg
    = GuestNoOp
    | TodayGuest Date
    | LoadedTokenGuest String


type LoggedinMsg
    = LoadedDays (Result Http.Error Days)
    | LoadedUser (Result Http.Error User)
    | Today Date
    | LoadedToken String


type LoadedMsg
    = UpdateAttributePart AttributeKey String
    | UpdateGoalPart ContentKey GoalKey
    | LoadedUserPart (Result Http.Error ())
    | ClickExpand ContentKey
    | ClickDiscardEdit ContentKey
    | ClickRemove ContentKey
    | CommitEdit
    | ToggleSidebar
    | AddElementShow ContentKeyW
    | AddElementSubmit
    | AddElementEdit ContentKeyW String
    | EditingElementPart ContentKeyW
    | ToggleDayTask Int Days.Task



-- | LoadedUserContent (Result Http.Error User)
-- type RequireDaysMsg
--     =
-- | UpdateGoal GoalKey
-- | UpdateDay DayKey


update : StateMsg -> StateModel -> ( StateModel, Cmd StateMsg )
update msg_ model_ =
    case ( model_, msg_ ) of
        ( Guest model, GuestMsg msg ) ->
            case msg of
                LoadedTokenGuest token ->
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

                -- ( _, AlwaysMsg msg ) ->
                --     case msg of
                Today date ->
                    let
                        range =
                            ( date, Date.add Date.Days -7 date )
                    in
                    -- update (LoadDays range) model
                    ( Loggedin { model | today = date, dayRange = range }, Cmd.none )

                LoadedToken token ->
                    let
                        key =
                            Session.navKey model.session
                    in
                    if token /= "" then
                        ( Loggedin { model | session = Session.fromString key (Just token) }
                        , Cmd.batch [ saveTokenPlanner token, Route.replaceUrl key Route.App ]
                        )

                    else
                        ( Loggedin model, Cmd.none )

        ( Loaded model, LoadedMsg msg ) ->
            updateLoaded msg model

        ( _, _ ) ->
            ( model_, Cmd.none )


updateLoaded : LoadedMsg -> LoadedModel -> ( StateModel, Cmd StateMsg )
updateLoaded msg model =
    let
        cmdnone model__ =
            Debug.log "Model update  " ( Loaded model__, Cmd.none )
    in
    case msg of
        ClickDiscardEdit key ->
            cmdnone
                { model
                    | viewState =
                        (\vs -> { vs | editing = Nothing, loading = Nothing }) model.viewState
                }

        ClickRemove key ->
            Debug.todo "not implemented clickremove"

        EditingElementPart element ->
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

                _ ->
                    Debug.todo "implement other editing"

        ClickExpand (AttributeKey index att) ->
            let
                vsNone =
                    { editing = Nothing, hovering = Nothing, loading = Nothing }

                key =
                    AttributeKey index att

                updateViewState cont =
                    (\vs -> { vs | editing = cont }) model.viewState

                -- if expanded then
                -- save to model, retract and expand other (key)
                vsNothing =
                    updateViewState Nothing

                vsNothingLoad =
                    { vsNothing | loading = Just ViewLLoading }

                vsKey =
                    updateViewState (Just key)

                vsKeyLoad =
                    { vsKey | loading = Just ViewLLoading }

                action =
                    case att.name_ of
                        "" ->
                            addAttribute model.token

                        _ ->
                            Debug.log "action patch" <| patchAttribute model.token
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
                        ( Loaded { modelNewUser | viewState = vsKeyLoad }, Cmd.batch [ action att_, editingPort () ] )

                Just (GoalKey index_ goal_) ->
                    let
                        modelNewUser =
                            { model | user = (\u -> { u | goals = Array.set index_ goal_ model.user.goals }) model.user }
                    in
                    {- update goal and expand -}
                    ( Loaded { modelNewUser | viewState = vsKeyLoad }, Cmd.batch [ patchGoals model.token goal_, editingPort () ] )

                _ ->
                    cmdnone model

        CommitEdit ->
            let
                model__ =
                    { model | viewState = (\vs_ -> { vs_ | editing = Nothing }) model.viewState }
            in
            case model.viewState.editing of
                Just (AttributeKey index att) ->
                    ( Loaded { model | user = (\u -> { u | attributes = Array.set index att model.user.attributes }) model.user }
                    , patchAttribute model.token att
                    )

                -- Just (GoalKey index goal) ->
                --     ( model__, Cmd.none )
                Nothing ->
                    cmdnone model__

                _ ->
                    Debug.todo "implement other"

        LoadedUserPart (Ok _) ->
            ( Loaded { model | viewState = (\vs -> { vs | loading = Just ViewLSuccess }) model.viewState }, loadUser model.token )

        LoadedUserPart (Err err) ->
            ( Loaded { model | viewState = (\vs -> { vs | loading = Just (ViewLError err) }) model.viewState }, loadUser model.token )

        AddElementShow (AttributeKeyW _) ->
            let
                att =
                    Attribute "" "" "" Nothing

                index =
                    Array.length model.user.attributes - 1
            in
            cmdnone { model | viewState = (\vs -> { vs | adding = Just <| AttributeKey index att }) model.viewState }

        AddElementShow (DayTaskKeyW index) ->
            let
                task =
                    Days.Task "" ""

                -- Days.Task "" "" Days.TSTodo Nothing
            in
            cmdnone { model | viewState = (\vs -> { vs | adding = Just <| DayTaskKey index task }) model.viewState }

        AddElementSubmit ->
            let
                ( model__, msg__ ) =
                    case model.viewState.adding of
                        Just (DayTaskKey index task) ->
                            let
                                maybeday =
                                    Array.get index model.days

                                -- days_ =
                            in
                            case maybeday of
                                Just day_ ->
                                    let
                                        tasks =
                                            Array.append day_.tasks <| Array.fromList [ task ]

                                        day =
                                            { day_ | tasks = tasks }

                                        days =
                                            Array.set index day model.days
                                    in
                                    ( { model | days = days }, patchDay model.token day )

                                _ ->
                                    ( model, Cmd.none )

                        _ ->
                            Debug.todo "implement other submit"
            in
            ( Loaded { model__ | viewState = (\vs -> { vs | adding = Nothing }) model.viewState }, msg__ )

        AddElementEdit key value ->
            case key of
                DayTaskKeyW index ->
                    let
                        task =
                            Days.Task value value

                        -- Days.Task value value Days.TSTodo Nothing
                    in
                    cmdnone { model | viewState = (\vs -> { vs | adding = Just <| DayTaskKey index task }) model.viewState }

                _ ->
                    Debug.todo "implement other edit"

        ToggleSidebar ->
            cmdnone { model | viewState = (\vs -> { vs | sidebarExpanded = not vs.sidebarExpanded }) model.viewState }

        _ ->
            cmdnone model



{- let
       maybetoken =
           Session.token model.session

       maybeuser =
           case model.user of
               UserLSuccess user ->
                   Just user

               _ ->
                   Nothing

       maybedays =
           case model.days of
               DaysLSuccess days ->
                   Just days

               _ ->
                   Nothing

       _ =
           Debug.log "message" msg

       cmdnone a =
           Debug.log "modelupdate" <| ( a, Cmd.none )
   in
   case msg of


               LoadedToken token ->
                   let
                       key =
                           Session.navKey model.session
                   in
                   if token /= "" then
                       ( { model | session = Session.fromString key (Just token) }
                       , Cmd.batch [ saveTokenPlanner token, Route.replaceUrl key Route.App ]
                       )

                   else
                       ( model, Cmd.none )

       msg_ ->
           case ( maybetoken, maybeuser, maybedays ) of
               ( Just token, Just user, Just days ) ->
                   case msg_ of


                       {- HoverEvent key hovering ->
                          if hovering then
                              cmdnone
                                  { model
                                      | viewState =
                                          (\vs -> { vs | hovering = Just key })
                                              model.viewState
                                  }

                          else
                              cmdnone
                                  { model
                                      | viewState =
                                          (\vs -> { vs | hovering = Nothing })
                                              model.viewState
                                  }
                       -}


                       -- Nothing ->
                       --     {- expand -}
                       --     ( { model | viewState = vsKey }, editingPort () )
                       -- UpdateDay part ->
                       --     Days.updatePart model part

                       -- ( { model | userPatchState = { editing = Nothing, state = UserLError Nothing } }, Cmd.none )
                       {- LoadDays ->
                              ( { model | dayLoadState = Days.LoadingStateWait }, loadDays token )

                          -- ( model, Cmd.none )
                          -- LoadedDays result ->
                              case result of
                                  Ok days ->
                                      ( { model | dayLoadState = Days.LoadingStateSuccess <| Just days }, Cmd.none )

                                  Err message ->
                                      ( { model | dayLoadState = Days.LoadingStateError <| Just message }, Cmd.none )
                       -}

                       {- AddAttributeShow ->
                          let
                              att =
                                  Attribute "" "" "" (Just "")

                              atts =
                                  Array.push att user.attributes

                              index =
                                  Array.length atts - 1

                              user_ =
                                  { user | attributes = atts }
                          in
                           cmdnone { model | user = UserLSuccess user_, viewState = (\vs -> { vs | editing = Just (AttributeKey index att) }) model.viewState }
                       -}


                       _ ->
                           ( model, Cmd.none )

               ( Just token, Just _, _ ) ->
                   case msg_ of
                       LoadDays ->
                           -- let
                           --     range =
                           --         ( Date.fromCalendarDate 2019 Jun 6, Date.fromCalendarDate 2019 Jun 4 )
                           -- in
                           ( { model | days = DaysLWait }, loadDays token model.viewState.dayRange )

                       LoadedDays (Ok days_) ->
                           cmdnone { model | days = DaysLSuccess days_ }

                       LoadedDays (Err err) ->
                           cmdnone { model | days = DaysLError err }

                       _ ->
                           cmdnone model

               ( _, _, _ ) ->
                   ( model, Cmd.none )
-}


addAttribute : String -> Attribute -> Cmd StateMsg
addAttribute token attribute =
    Http.request
        { method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeAttribute attribute
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me/attributes"
        , expect = Http.expectWhatever (LoadedMsg << LoadedUserPart)
        }


patchAttribute : String -> Attribute -> Cmd StateMsg
patchAttribute token attribute =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeAttribute attribute
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me/attributes/" ++ attribute.name_
        , expect = Http.expectWhatever (LoadedMsg << LoadedUserPart)
        }


patchGoals : String -> Goal -> Cmd StateMsg
patchGoals token goal =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeGoal goal
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me/goals/" ++ goal.name_
        , expect = Http.expectWhatever (LoadedMsg << LoadedUserPart)
        }


patchDay : String -> Day -> Cmd StateMsg
patchDay token day =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| Days.encodeDay day
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me/days/" ++ Date.format "YYYYMMdd" day.date_
        , expect = Http.expectWhatever (LoadedMsg << LoadedUserPart)
        }



-- View


view : StateModel -> List (Html StateMsg)
view model_ =
    case model_ of
        Guest _ ->
            [ div [ class "planner container" ]
                [ div [ class "planner box" ]
                    [ p [ class "planner error-text" ] [ text "You have to be logged in to view this page." ]
                    , a [ class "planner link-button", Route.href Route.Login ] [ text "Login" ]
                    ]
                ]
            ]

        Loggedin _ ->
            [ div [ class "planner container" ]
                [ div [ class "planner box" ]
                    [ p [ class "planner loading-text" ] [ text "Loading content..." ] ]
                ]
            ]

        Loaded model ->
            List.map (Html.map LoadedMsg)
                [ div [ class "planner header" ] [ text model.user.email ]
                , viewSidebar model.viewState model.user
                , div [ class "planner planner-body" ]
                    [ button [ class "planner sidebar-expand", onClick ToggleSidebar ] [ text ">" ]
                    , viewDays model.days model.viewState
                    ]
                ]


viewSidebar : ViewState -> User -> Html LoadedMsg
viewSidebar viewState user =
    div
        [ class <|
            "planner sidebar "
                ++ (if viewState.sidebarExpanded then
                        "show"

                    else
                        "hide"
                   )
        ]
        [ div [ class "planner sidebar-header" ]
            [ i [ class "fas fa-user-circle", title user.email ] []
            , h2 [ class "planner sidebar-header-text" ] [ text "daycare" ]
            , viewSyncIndicator viewState.loading
            ]
        , hr [ class "planner sidebar" ] []
        , div [ class "planner sidebar-body" ]
            [ -- text ("email: " ++ user.email)
              viewAttributes user viewState
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
                    , title "Error! Could not sync."
                    ]
                    []

            _ ->
                i
                    [ class "planner fas fa-check-circle"
                    , title "Up to date!"
                    ]
                    []
        ]


viewDays : Array Day -> ViewState -> Html LoadedMsg
viewDays days viewState =
    div [ class "planner days" ]
        -- (case daysLs of
        --     DaysLSuccess days ->
        (Array.indexedMap (viewDay viewState) days
            |> Array.toList
        )



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
    let
        desc =
            Maybe.withDefault "no description" day.description

        date =
            day.date
                |> Date.format "dd.MM.YYYY"

        adding =
            Debug.log "adding" <|
                case viewState.adding of
                    Just (DayTaskKey index_ _) ->
                        index == index_

                    _ ->
                        False
    in
    div [ class " planner day" ]
        [ div
            [ class "planner day-header" ]
            [ h4 [ class "planner day-date" ] [ text date ]
            , p [ class "planner day-description" ] [ text desc ]
            ]
        , div [ class "planner day-body" ]
            [ viewTasks index day adding ]
        ]


viewTasks : Int -> Days.Day -> Bool -> Html LoadedMsg
viewTasks index day adding =
    ul [ class "planner day-tasks" ]
        ((Array.map (viewTask index) day.tasks
            |> Array.toList
         )
            -- ++ [ div [ class "planner day-task add" ]
            ++ [ if adding then
                    li [ class "planner day-task adding" ]
                        [ Html.form [ class "planner add-task-form", onSubmit AddElementSubmit ]
                            [ input [ class "planner add-task-input", onInput (AddElementEdit (DayTaskKeyW index)), placeholder "task", autofocus True ] [] ]
                        ]

                 else
                    li [ class "planner day-task add-task" ]
                        [ i [ class "fas fa-plus", onClick (AddElementShow <| DayTaskKeyW index) ] []
                        ]
               ]
        )



-- |> List.reverse
--    ]


viewTask : Int -> Days.Task -> Html LoadedMsg
viewTask index task =
    li [ class "planner day-task", onClick (ToggleDayTask index task) ] [ text task.name ]


viewResponseHttpError : Http.Error -> String
viewResponseHttpError err =
    case err of
        -- IMPLEMENT : MAYBE HTTP ERROR -> HTTP ERROR
        Http.BadStatus code ->
            -- text ("Could not get token. Status " ++ String.fromInt code)
            case code of
                401 ->
                    "wrong identifier or password"

                500 ->
                    "server error"

                _ ->
                    "http error status " ++ String.fromInt code

        Http.NetworkError ->
            "could not connect to server"

        _ ->
            Debug.toString err


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
            [ div [ class "planner attribute-add-button", onClick (AddElementShow <| AttributeKeyW Nothing) ]
                [ i [ class "planner attribute-add-icon fas fa-plus" ] []
                ]
            ]
        ]


viewAttribute : Int -> Attribute -> Maybe ViewLoadState -> Maybe ContentKey -> Html LoadedMsg
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
                        [ onInput <| (EditingElementPart << AttributeKeyW << Just << AShort)
                        , value att_.short
                        , class "planner attribute-short"
                        ]
                        []

                nameinput =
                    input
                        [ onInput <| (EditingElementPart << AttributeKeyW << Just << AName)
                        , value att_.name
                        , class "planner attribute-name"
                        ]
                        []

                descinput =
                    textarea
                        [ onInput <|
                            (EditingElementPart << AttributeKeyW << Just << ADescription)
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
                [ Html.form [ onSubmit <| CommitEdit ]
                    [ div [ class "planner attribute-editing-header" ]
                        [ shortinput
                        , nameinput

                        -- , if untouched then
                        --     {- untouched editing -}
                        --     text ""
                        --   else
                        --     {- touched editing -}
                        , div [ class "planner buttons editing" ]
                            [ button [ type_ "button", onClick (ClickRemove key), class "planner fas fa-trash" ] []
                            , button [ type_ "button", onClick (ClickDiscardEdit key), class "planner fas fa-times" ] []
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
                    , h3 [ class "planner attribute-name", onClick (ClickExpand key) ] [ text att.name ]
                    , div [ class "planner attribute-desc-wrapper" ]
                        [ span [ class "planner attribute-desc" ] [ description ] ]
                    ]
                , div [ class "planner buttons" ] [ button [ class "planner fas fa-pen", onClick (ClickExpand key), tabindex 0 ] [] ]
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



{-
   viewDays : Days.LoadState -> PatchState -> Html StateMsg
   viewDays dayLoadState patchState =
       div []
           [ let
               ( btntext, btnwait ) =
                   case dayLoadState of
                       Days.LoadingStateIdle ->
                           ( "Load days", False )

                       Days.LoadingStateWait ->
                           ( "Loading...", True )

                       _ ->
                           ( "Reload days", False )
             in
             button [ onClick LoadDays, disabled btnwait ] [ text btntext ]
           , case dayLoadState of
               Days.LoadingStateError err ->
                   div []
                       [ text "An Error occurred while fetching days."
                       , text <| " error: " ++ Debug.toString err
                       ]

               Days.LoadingStateSuccess days ->
                   Days.viewDays days

               _ ->
                   Html.nothing
           ]
-}


toSession : StateModel -> Session
toSession model =
    case model of
        Guest guest ->
            guest.session

        Loggedin loggedin ->
            loggedin.session

        Loaded loaded ->
            loaded.session


getToday : StateModel -> Cmd StateMsg
getToday model =
    let
        msg =
            case model of
                Guest _ ->
                    GuestMsg << TodayGuest

                _ ->
                    LoggedinMsg << Today
    in
    Task.perform msg Date.today



-- Http


loadUser : String -> Cmd StateMsg
loadUser token =
    Http.request
        { method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me"
        , expect = Http.expectJson (LoggedinMsg << LoadedUser) decodeUser
        }


loadDays : String -> ( Date, Date ) -> Cmd StateMsg
loadDays token range =
    Http.request
        { method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me/days"
        , expect = Http.expectJson (LoggedinMsg << LoadedDays) Days.decodeDays
        }



-- Json


encodeAttribute : Attribute -> E.Value
encodeAttribute attribute =
    let
        desc =
            Maybe.withDefault "" attribute.description
    in
    E.object
        [ ( "short", E.string attribute.short )
        , ( "name", E.string attribute.name )
        , ( "description", E.string desc )
        ]


encodeGoal : Goal -> E.Value
encodeGoal goal =
    E.object
        [ ( "name", E.string goal.name )
        , ( "description", E.string (Maybe.withDefault "" goal.description) )
        ]


decodeUser : D.Decoder User
decodeUser =
    D.map4 User
        (D.field "email" D.string)
        (D.field "attributes" (D.array decodeAttribute))
        (D.field "goals" (D.array decodeGoal))
        (D.field "level" decodeLevel)


decodeAttribute : D.Decoder Attribute
decodeAttribute =
    D.map4 Attribute
        (D.field "short" D.string)
        (D.field "short" D.string)
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))


decodeAttributeKey : D.Decoder (Maybe ContentKey)
decodeAttributeKey =
    D.map Just <| D.map (AttributeKey 0) decodeAttribute


decodeGoal : D.Decoder Goal
decodeGoal =
    D.map4 Goal
        (D.field "name" D.string)
        (D.field "name" D.string)
        (D.maybe <| D.field "description" D.string)
        (D.maybe <| D.field "deadline" D.string)


decodeGoalKey : D.Decoder ContentKey
decodeGoalKey =
    D.map (GoalKey 0) decodeGoal


decodeLevel : D.Decoder UserLevel
decodeLevel =
    D.string
        |> D.andThen
            (\level ->
                case level of
                    "user" ->
                        D.succeed LevelUser

                    "mod" ->
                        D.succeed LevelMod

                    "admin" ->
                        D.succeed LevelAdmin

                    _ ->
                        D.fail "no valid level"
            )



-- toSession : Model -> Session
-- toSession model =
--     model.session


port submitEditPort : (() -> msg) -> Sub msg


port editingPort : () -> Cmd msg


port loadedTokenPlanner : (String -> msg) -> Sub msg


port saveTokenPlanner : String -> Cmd msg


subscriptions : StateModel -> Sub StateMsg
subscriptions model =
    let
        msg =
            case model of
                Guest _ ->
                    GuestMsg << LoadedTokenGuest

                _ ->
                    LoggedinMsg << LoadedToken
    in
    Sub.batch
        [ submitEditPort (\_ -> LoadedMsg <| CommitEdit)
        , loadedTokenPlanner msg
        ]
