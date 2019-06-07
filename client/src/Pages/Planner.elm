port module Pages.Planner exposing (Attribute, AttributeKey(..), ContentKey(..), DayKey(..), EditState(..), Goal, GoalKey(..), Model, Msg(..), User, UserLoadState(..), UserPart(..), UserPatchState, ViewLoadState(..), ViewState, decodeAttribute, decodeAttributeKey, decodeGoal, decodeGoalKey, decodeUserLoadState, editingPort, encodeAttribute, encodeGoal, indexFromPart, init, loadDays, loadUser, submitEditPort, subscriptions, toSession, update, updateAttribute, updateAttributePart, updateAttributeSend, updateAttributes, updateGoalPart, updateGoalSend, updateGoals, view, viewAttribute, viewAttributes, viewGoal, viewGoals, viewResponseHttpError)

import Array exposing (Array)
import Days exposing (Date, Day, Days, Task)
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


type alias Model =
    { session : Session
    , user : UserLoadState
    , viewState : ViewState
    , errormsg : Maybe String
    }



-- , userPatchState : UserPatchState
-- , days : Days.Model


type UserLoadState
    = UserLIdle
    | UserLWait
    | UserLError Http.Error
    | UserLSuccess User


type alias UserPatchState =
    { state : UserLoadState
    , editing : Maybe EditState
    }


type alias ViewState =
    { loading : Maybe ViewLoadState
    , hovering : Maybe ContentKey
    , editing : Maybe ContentKey
    , sidebarExpanded : Bool
    }


type ViewLoadState
    = ViewLLoading
    | ViewLError
    | ViewLSuccess


type alias User =
    { email : String
    , attributes : Array Attribute
    , goals : Array Goal
    }


type alias Attribute =
    { short_ : String
    , short : String
    , name : String
    , description : Maybe String
    }


type alias Goal =
    { name_ : String
    , name : String
    , description : Maybe String
    , deadline : Maybe String
    }


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
    = AShort Int
    | AName Int
    | ADescription Int
    | ASend Int


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


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , user = UserLIdle

            -- , userPatchState =
            --     { state = UserLIdle
            --     , editing = Nothing
            --     }
            , viewState =
                { hovering = Nothing
                , editing = Nothing
                , loading = Nothing
                , sidebarExpanded = False
                }

            -- , dayLoadState = Days.LoadingStateIdle
            , errormsg = Nothing
            }

        maybetoken =
            Session.cred session
    in
    case maybetoken of
        Nothing ->
            ( model, Cmd.none )

        Just token ->
            ( { model | user = UserLWait }
            , loadUser token
            )



-- Update


type Msg
    = LoadedToken String
    | UserLoginLoadedLoadUserContent (Result Http.Error String)
    | LoadUserContent
    | LoadedUserContent (Result Http.Error User)
    | UpdateAttributePart AttributeKey String
    | UpdateGoalPart ContentKey GoalKey
    | EditingAttributePart AttributeKey String
    | LoadedUserPart (Result Http.Error ContentKey)
    | LoadDays
    | LoadedDays (Result Http.Error Days)
    | HoverEvent ContentKey Bool
    | ClickExpand ContentKey
    | ClickDiscardEdit ContentKey
    | ClickRemove ContentKey
    | CommitEdit
    | ToggleSidebar



-- | UpdateGoal GoalKey
-- | UpdateDay DayKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        maybetoken =
            Session.cred model.session

        maybecontent =
            case model.user of
                UserLSuccess content ->
                    Just content

                _ ->
                    Nothing
    in
    case ( maybetoken, maybecontent ) of
        ( Just token, Nothing ) ->
            case msg of
                LoadUserContent ->
                    ( { model | user = UserLWait }, loadUser token )

                LoadedUserContent (Ok content) ->
                    ( { model | user = UserLSuccess content }, Cmd.none )

                LoadedUserContent (Err message) ->
                    ( { model | user = UserLError message }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( Just token, Just user ) ->
            let
                cmdnone a =
                    ( a, Cmd.none )
            in
            case msg of
                ClickDiscardEdit key ->
                    cmdnone
                        { model
                            | viewState =
                                (\vs -> { vs | editing = Nothing, loading = Nothing }) model.viewState
                        }

                ClickRemove key ->
                    let
                        attributes =
                            ""
                    in
                    cmdnone model

                EditingAttributePart part value ->
                    let
                        maybeAttribute =
                            Array.get index user.attributes

                        index =
                            indexFromPart part
                    in
                    case maybeAttribute of
                        Just attribute ->
                            let
                                userNewattribute =
                                    case part of
                                        AName _ ->
                                            { attribute | name = value }

                                        ADescription _ ->
                                            { attribute | description = Just value }

                                        _ ->
                                            Debug.todo "implement other"
                            in
                            cmdnone
                                { model
                                    | viewState =
                                        (\vs ->
                                            { vs | editing = Just (AttributeKey index userNewattribute) }
                                        )
                                            model.viewState
                                }

                        _ ->
                            cmdnone model

                HoverEvent key hovering ->
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
                    in
                    case model.viewState.editing of
                        Just (AttributeKey index_ att_) ->
                            let
                                userNewAttributes =
                                    { user | attributes = Array.set index_ att_ user.attributes }
                            in
                            if index == index_ && att == att_ then
                                {- dont update but retract -}
                                cmdnone { model | viewState = vsNothing }

                            else if index == index_ then
                                {- update and retract -}
                                ( { model | user = UserLSuccess userNewAttributes, viewState = vsNothingLoad }, updateAttributes token att_ )

                            else
                                {- update and expand -}
                                ( { model | user = UserLSuccess userNewAttributes, viewState = vsKeyLoad }, Cmd.batch [ updateAttributes token att_, editingPort () ] )

                        Just (GoalKey index_ goal_) ->
                            let
                                newGoals =
                                    { user | goals = Array.set index_ goal_ user.goals }
                            in
                            {- update goal and expand -}
                            ( { model | user = UserLSuccess newGoals, viewState = vsKeyLoad }, Cmd.batch [ updateGoals token goal_, editingPort () ] )

                        Nothing ->
                            {- expand -}
                            ( { model | viewState = vsKey }, editingPort () )

                -- UpdateDay part ->
                --     Days.updatePart model part
                CommitEdit ->
                    let
                        model_ =
                            { model | viewState = (\vs_ -> { vs_ | editing = Nothing }) model.viewState }
                    in
                    case model.viewState.editing of
                        Just (AttributeKey index att) ->
                            let
                                userNewAttributes =
                                    { user | attributes = Array.set index att user.attributes }
                            in
                            ( { model_ | user = UserLSuccess userNewAttributes }, updateAttributes token att )

                        Just (GoalKey index goal) ->
                            ( model_, Cmd.none )

                        Nothing ->
                            cmdnone model

                LoadedUserPart (Ok key) ->
                    let
                        up =
                            (\vs -> { vs | loading = Just ViewLSuccess }) model.viewState
                    in
                    ( { model | viewState = up }, loadUser token )

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
                ToggleSidebar ->
                    cmdnone { model | viewState = (\vs -> { vs | sidebarExpanded = not vs.sidebarExpanded }) model.viewState }

                _ ->
                    ( model, Cmd.none )

        ( Nothing, _ ) ->
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

                _ ->
                    ( model, Cmd.none )



-- ( _, _ ) ->
--     ( model, Cmd.none )


updateAttributePart : Model -> Int -> AttributeKey -> String -> ( Model, Cmd Msg )
updateAttributePart model index attKey value =
    case model.user of
        UserLSuccess content ->
            let
                eCont =
                    case Array.get index content.attributes of
                        Just att ->
                            case attKey of
                                AShort _ ->
                                    { content | attributes = Array.set index { att | short = value } content.attributes }

                                AName _ ->
                                    { content | attributes = Array.set index { att | name = value } content.attributes }

                                ADescription _ ->
                                    { content | attributes = Array.set index { att | description = Just value } content.attributes }

                                _ ->
                                    content

                        Nothing ->
                            content

                up =
                    (\vs ->
                        { vs
                            | editing =
                                case Array.get index content.attributes of
                                    Just att ->
                                        Just (AttributeKey index att)

                                    Nothing ->
                                        Nothing
                        }
                    )
                        model.viewState
            in
            ( { model | user = UserLSuccess eCont, viewState = up }, Cmd.none )

        _ ->
            ( { model | user = UserLError Http.NetworkError }, Cmd.none )


updateAttributeSend : Model -> String -> Int -> ( Model, Cmd Msg )
updateAttributeSend model token index =
    let
        up =
            (\vs -> { vs | loading = Nothing }) model.viewState
    in
    case model.user of
        UserLSuccess content ->
            let
                att =
                    Array.get index content.attributes
            in
            case att of
                Just value ->
                    ( { model | viewState = up }, updateAttributes token value )

                Nothing ->
                    ( { model | viewState = up }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateAttribute : User -> Int -> Attribute -> User
updateAttribute user index att =
    { user | attributes = Array.set index att user.attributes }


updateGoalPart : Model -> Int -> String -> GoalKey -> ( Model, Cmd Msg )
updateGoalPart model index value goalPart =
    case model.user of
        UserLSuccess content ->
            let
                maybegoal =
                    Array.get index content.goals

                eCont =
                    case maybegoal of
                        Just goal ->
                            case goalPart of
                                GName _ ->
                                    { content | goals = Array.set index { goal | name = value } content.goals }

                                GDescription _ ->
                                    { content | goals = Array.set index { goal | description = Just value } content.goals }

                                _ ->
                                    Debug.todo "implement other goal parts"

                        Nothing ->
                            content

                up =
                    (\vs ->
                        { vs
                            | editing =
                                case maybegoal of
                                    Just goal_ ->
                                        Just (GoalKey index goal_)

                                    Nothing ->
                                        Nothing
                        }
                    )
                        model.viewState
            in
            ( { model | user = UserLSuccess eCont, viewState = up }, Cmd.none )

        _ ->
            ( { model | user = UserLError Http.NetworkError }, Cmd.none )


updateGoalSend : Model -> String -> Int -> ( Model, Cmd Msg )
updateGoalSend model token index =
    let
        up =
            { model | viewState = (\vs -> { vs | loading = Nothing }) model.viewState }
    in
    case model.user of
        UserLSuccess content ->
            let
                att =
                    Array.get index content.goals
            in
            case att of
                Just value ->
                    ( up, updateGoals token value )

                Nothing ->
                    ( up, Cmd.none )

        _ ->
            ( model, Cmd.none )


indexFromPart : AttributeKey -> Int
indexFromPart part =
    case part of
        AShort i ->
            i

        AName i ->
            i

        ADescription i ->
            i

        ASend i ->
            i


updateAttributes : String -> Attribute -> Cmd Msg
updateAttributes token attribute =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeAttribute attribute
        , headers = [ Http.header "token" token ]
        , url = "http://localhost:5000/api/v3/users/me/attributes/" ++ attribute.short_
        , expect = Http.expectJson LoadedUserPart decodeAttributeKey
        }


updateGoals : String -> Goal -> Cmd Msg
updateGoals token goal =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeGoal goal
        , headers = [ Http.header "token" token ]
        , url = "http://localhost:5000/api/v3/users/me/goals/" ++ goal.name_
        , expect = Http.expectJson LoadedUserPart decodeGoalKey
        }



-- View


view : Model -> List (Html Msg)
view model =
    case model.user of
        UserLSuccess user ->
            [ div
                [ class <|
                    "planner sidebar "
                        ++ (if model.viewState.sidebarExpanded then
                                "show"

                            else
                                "hide"
                           )
                ]
                [ div [ class "planner sidebar-header" ]
                    [ h2 [ class "planner sidebar-header-text" ] [ text "daycare" ]
                    , viewSyncIndicator model.viewState.loading
                    ]
                , div [ class "planner sidebar-body" ]
                    [ -- text ("email: " ++ user.email)
                      viewAttributes user model.viewState
                    ]
                ]
            , div [ class "planner planner-body" ]
                [ button [ class "planner sidebar-expand", onClick ToggleSidebar ] [ text ">" ]
                , viewDays
                ]
            ]

        UserLWait ->
            [ div [ class "planner container" ]
                [ div [ class "planner box" ]
                    [ p [ class "planner loading-text" ] [ text "Loading content..." ] ]
                ]
            ]

        UserLError error ->
            [ div [ class "planner container" ]
                [ div [ class "planner box" ]
                    [ p [ class "planner error-text" ]
                        [ text <| "Error (" ++ viewResponseHttpError error ++ ")" ]
                    , a [ class "planner link-button", Route.href Route.App ]
                        [ i [ class "planner fas fa-sync-alt" ] [], text "Refresh" ]
                    ]
                ]
            ]

        UserLIdle ->
            [ div [ class "planner container" ]
                [ div [ class "planner error-container" ]
                    [ p [ class "planner error-text" ] [ text "You have to be logged in to view this page." ]
                    , a [ class "planner link-button", Route.href Route.Login ] [ text "Login" ]
                    ]
                ]
            ]



-- _ ->
--     [ div [ class "planner error-container" ]
--         [ text "An unknown Error occurred" ]
--     ]


viewSyncIndicator : Maybe ViewLoadState -> Html Msg
viewSyncIndicator loading =
    div [ class "planner sync-indicator" ]
        [ case loading of
            Just ViewLLoading ->
                i
                    [ class "planner fas fa-spinner"
                    , title "Loading..."
                    ]
                    []

            Just ViewLError ->
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


viewDays =
    div [ class "planner days" ]
        [ div [ class " planner day" ]
            [ div
                [ class "planner day-header" ]
                [ h4 [ class "planner day-date" ] [ text "26.05.2019" ]
                , p [ class "planner day-description" ] [ text "not much" ]
                ]
            , div [ class "planner day-body" ]
                [ ul [ class "planner day-tasks" ]
                    [ li [ class "planner day-task" ] [ text "raus" ]
                    , li [ class "planner day-task" ] [ text "daycare" ]
                    ]
                ]
            ]
        ]


viewResponseHttpError : Http.Error -> String
viewResponseHttpError err =
    case err of
        -- IMPLEMENT : MAYBE HTTP ERROR -> HTTP ERROR
        Http.BadStatus code ->
            -- text ("Could not get token. Status " ++ String.fromInt code)
            case code of
                401 ->
                    "Wrong Username or Password"

                500 ->
                    "Server Error"

                _ ->
                    "Status " ++ String.fromInt code

        Http.NetworkError ->
            "Could not connect to server"

        _ ->
            Debug.toString err


viewAttributes : User -> ViewState -> Html Msg
viewAttributes user viewState =
    user.attributes
        |> Array.indexedMap
            (\index att -> lazy4 viewAttribute index att viewState.loading viewState.editing)
        |> Array.toList
        -- |> ul []
        -- |> lazy
        -- |> List.singleton
        |> div [ class "planner attributes" ]


viewAttribute : Int -> Attribute -> Maybe ViewLoadState -> Maybe ContentKey -> Html Msg
viewAttribute index att loading editing =
    let
        patching =
            loading /= Nothing

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
                        [ onInput (EditingAttributePart (AShort index))
                        , value att_.short
                        , class "planner attribute-short"
                        ]
                        []

                nameinput =
                    input
                        [ onInput (EditingAttributePart (AName index))
                        , value att_.name
                        , class "planner attribute-name"
                        ]
                        []

                descinput =
                    textarea
                        [ onInput
                            (EditingAttributePart (ADescription index))
                        , class "planner attribute-desc"
                        ]
                        [ description ]

                key_ =
                    AttributeKey index att_
            in
            div
                [ onMouseOver <| HoverEvent key True
                , onMouseOut <| HoverEvent key False
                , class "planner attribute editing"
                ]
                [ Html.form [ onSubmit (ClickExpand key) ]
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
                [ onMouseOver <| HoverEvent key True
                , onMouseOut <| HoverEvent key False
                , class "planner attribute draggable"
                , id <| "att-" ++ String.fromInt index
                , draggable "true"
                , attribute "ondragstart" "drag(event)"
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


viewGoals : User -> UserPatchState -> Html Msg
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


viewGoal : Int -> Goal -> UserPatchState -> Html Msg
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
   viewDays : Days.LoadState -> PatchState -> Html Msg
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
-- Http


loadUser : String -> Cmd Msg
loadUser token =
    Http.request
        { method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , headers = [ Http.header "token" token ]
        , url = "http://localhost:5000/api/v3/users/me"
        , expect = Http.expectJson LoadedUserContent decodeUserLoadState
        }


loadDays : String -> Cmd Msg
loadDays token =
    Http.request
        { method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , headers = [ Http.header "token" token ]
        , url = "http://localhost:5000/api/v3/users/me/days"
        , expect = Http.expectJson LoadedDays Days.decodeDays
        }



-- Json


encodeAttribute : Attribute -> E.Value
encodeAttribute attribute =
    case attribute.description of
        Just desc ->
            E.object
                [ ( "short", E.string attribute.short )
                , ( "name", E.string attribute.name )
                , ( "description", E.string desc )
                ]

        Nothing ->
            E.object
                [ ( "short", E.string attribute.short )
                , ( "name", E.string attribute.name )
                ]


encodeGoal : Goal -> E.Value
encodeGoal goal =
    case goal.description of
        Just desc ->
            E.object
                [ ( "name", E.string goal.name )
                , ( "description", E.string desc )
                ]

        Nothing ->
            E.object
                [ ( "name", E.string goal.name )
                ]


decodeUserLoadState : D.Decoder User
decodeUserLoadState =
    D.map3 User
        (D.field "email" D.string)
        (D.field "attributes" (D.array decodeAttribute))
        (D.field "goals" (D.array decodeGoal))


decodeAttribute : D.Decoder Attribute
decodeAttribute =
    D.map4 Attribute
        (D.field "short" D.string)
        (D.field "short" D.string)
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))


decodeAttributeKey : D.Decoder ContentKey
decodeAttributeKey =
    D.map (AttributeKey 0) decodeAttribute


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


toSession : Model -> Session
toSession model =
    model.session


port submitEditPort : (() -> msg) -> Sub msg


port editingPort : () -> Cmd msg


port loadedTokenPlanner : (String -> msg) -> Sub msg


port saveTokenPlanner : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ submitEditPort (\_ -> CommitEdit)
        , loadedTokenPlanner LoadedToken
        ]
