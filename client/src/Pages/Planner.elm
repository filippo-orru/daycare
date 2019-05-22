module Pages.Planner exposing (Attribute, AttributeKey(..), DayKey(..), EditState(..), Goal, GoalKey(..), Model, Msg(..), User, UserLoadState(..), UserPart(..), decodeAttribute, decodeGoal, decodeUserLoadState, encodeAttribute, encodeGoal, init, loadDays, loadUser, toSession, update, updateAttributePart, updateAttributeSend, updateAttributes, updateGoalPart, updateGoalSend, updateGoals, view, viewAttribute, viewAttributes, viewGoal, viewGoals, viewResponseHttpError)

import Array exposing (Array)
import Days exposing (Date, Day, Days, Task)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver, onSubmit)
import Html.Extra as Html
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Http
import Json.Decode as D
import Json.Encode as E
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    , userLoadState : UserLoadState
    , viewState : ViewState
    , errormsg : Maybe String
    }



-- , userPatchState : UserPatchState
-- , days : Days.Model


type UserLoadState
    = UserLoadingStateIdle
    | UserLoadingStateWait
    | UserLoadingStateError (Maybe Http.Error)
    | UserLoadingStateSuccess (Maybe User)


type alias UserPatchState =
    { state : UserLoadState
    , editing : Maybe EditState
    }


type alias ViewState =
    { loading : Maybe ContentKey
    , hovering : Maybe ContentKey
    , editing : Maybe ContentKey
    }


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
            , userLoadState = UserLoadingStateIdle

            -- , userPatchState =
            --     { state = UserLoadingStateIdle
            --     , editing = Nothing
            --     }
            , viewState =
                { hovering = Nothing
                , editing = Nothing
                , loading = Nothing
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
            ( { model | userLoadState = UserLoadingStateWait }
            , loadUser token
            )



-- Update


type Msg
    = UserLoginLoadedLoadUserContent (Result Http.Error String)
    | LoadUserContent
    | LoadedUserContent (Result Http.Error User)
    | UpdateAttributePart AttributeKey String
    | UpdateGoalPart ContentKey GoalKey
    | EditingAttributePart AttributeKey String
    | LoadedUserPart (Result Http.Error ContentKey)
    | LoadDays
    | LoadedDays (Result Http.Error Days)
    | HoverEvent ContentKey Bool
    | ClickExpand ContentKey Bool
    | ClickDiscardEdit ContentKey



-- | UpdateGoal GoalKey
-- | UpdateDay DayKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        maybetoken =
            Session.cred model.session

        maybecontent =
            case model.userLoadState of
                UserLoadingStateSuccess content ->
                    Just content

                _ ->
                    Nothing
    in
    case ( maybetoken, maybecontent ) of
        ( Just token, Nothing ) ->
            case msg of
                LoadUserContent ->
                    ( { model | userLoadState = UserLoadingStateWait }, loadUser token )

                LoadedUserContent (Ok content) ->
                    ( { model | userLoadState = UserLoadingStateSuccess (Just content) }, Cmd.none )

                LoadedUserContent (Err message) ->
                    ( { model | userLoadState = UserLoadingStateError <| Just message }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( Just token, Just (Just user) ) ->
            let
                cmdnone a =
                    ( a, Cmd.none )
            in
            case msg of
                {- UpdateAttributePart key value ->
                       updateAttributePart model value key

                   UpdateGoalPart (GoalKey index goal) key ->
                       updateGoalPart model index key value

                    UpdateAttribute (AName index value) ->
                          updateAttributePart model index value (AName index value)

                      UpdateAttribute (ADescription index value) ->
                          updateAttributePart model index value (ADescription index value)

                      UpdateAttribute (ASend index) ->
                          updateAttributeSend model token index

                      UpdateGoal (GName index value) ->
                          updateGoalPart model index value (GName index value)

                      UpdateGoal (GDescription index value) ->
                          updateGoalPart model index value (GDescription index value)

                      UpdateGoal (GSend index) ->
                          updateGoalSend model token index
                -}
                ClickDiscardEdit key ->
                    cmdnone
                        { model
                            | viewState =
                                (\vs -> { vs | editing = Nothing, loading = Nothing }) model.viewState
                        }

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
                                newattribute =
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
                                            { vs | editing = Just (AttributeKey index newattribute) }
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

                ClickExpand key expanded ->
                    let
                        vsNone =
                            { editing = Nothing, hovering = Nothing, loading = Nothing }
                    
                        case key of
                            AttributeKey index att ->
-- todo better differentiaton between outcomes with at least one case to catch touched/untouched diff
                        model_ =
                            { model
                                | userLoadState =
                                    UserLoadingStateSuccess <| Just (updateAttribute user index att)
                            }
                    in
                    if expanded && model.viewState.editing == Just key then
                        Debug.todo "save to model and retract"
                            cmdnone
                            { model_ | viewState = vsNone }

                    else if expanded then
                        Debug.todo "save to model and expand other (key)"
                            cmdnone
                            { model_ | viewState = (\vs -> { vs | editing = key }) model.viewState }

                    else
                        Debug.todo "expand"
                            cmdnone
                            { model | viewState = (\vs -> { vs | editing = key }) model.viewstate }

                {- case model.viewState.editing of
                   Just (AttributeKey index_ att) ->
                       if key == AttributeKey index_ att then
                           cmdnone { model | viewState = vsNone }

                       else if index /= index_ then
                           cmdnone
                               { model_ | viewState = (\vs -> { vs | editing = Just key }) vsNone }

                       else
                           ( { model_ | viewState = vsNone }, updateAttributes token att )

                   _ ->
                       cmdnone { model | viewState = (\vs -> { vs | editing = Just key }) model.viewState }
                -}
                -- UpdateDay part ->
                --     Days.updatePart model part
                LoadedUserPart (Ok key) ->
                    let
                        up =
                            (\vs ->
                                { vs
                                    | loading = Just key
                                    , editing = Nothing
                                }
                            )
                                model.viewState
                    in
                    ( { model | viewState = up }, loadUser token )

                -- ( { model | userPatchState = { editing = Nothing, state = UserLoadingStateError Nothing } }, Cmd.none )
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
                _ ->
                    ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


updateAttributePart : Model -> Int -> AttributeKey -> String -> ( Model, Cmd Msg )
updateAttributePart model index attKey value =
    case model.userLoadState of
        UserLoadingStateSuccess (Just content) ->
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
            ( { model | userLoadState = UserLoadingStateSuccess (Just eCont), viewState = up }, Cmd.none )

        _ ->
            ( { model | userLoadState = UserLoadingStateError <| Just Http.NetworkError }, Cmd.none )


updateAttributeSend : Model -> String -> Int -> ( Model, Cmd Msg )
updateAttributeSend model token index =
    let
        up =
            (\vs -> { vs | loading = Nothing }) model.viewState
    in
    case model.userLoadState of
        UserLoadingStateSuccess (Just content) ->
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
    case model.userLoadState of
        UserLoadingStateSuccess (Just content) ->
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
            ( { model | userLoadState = UserLoadingStateSuccess (Just eCont), viewState = up }, Cmd.none )

        _ ->
            ( { model | userLoadState = UserLoadingStateError <| Just Http.NetworkError }, Cmd.none )


updateGoalSend : Model -> String -> Int -> ( Model, Cmd Msg )
updateGoalSend model token index =
    let
        up =
            { model | viewState = (\vs -> { vs | loading = Nothing }) model.viewState }
    in
    case model.userLoadState of
        UserLoadingStateSuccess (Just content) ->
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
    case model.userLoadState of
        UserLoadingStateSuccess (Just user) ->
            let
                editing =
                    model.viewState.editing
            in
            [ div []
                [ text ("email: " ++ user.email) ]
            , viewAttributes user model.viewState
            , div [] []

            -- , viewGoals user model.viewState
            -- , div [] []
            -- , viewDays model.dayLoadState model.userPatchState
            ]

        UserLoadingStateError error ->
            [ div [] (viewResponseHttpError error) ]

        UserLoadingStateWait ->
            [ text "Loading content..." ]

        UserLoadingStateIdle ->
            [ text "You have to be logged in to view this page.", div [] [ a [ Route.href Route.Login ] [ text "Login" ] ] ]

        _ ->
            [ div [] [ text "An unknown Error occurred" ] ]


viewResponseHttpError : Maybe Http.Error -> List (Html Msg)
viewResponseHttpError err =
    [ case err of
        Just error ->
            case error of
                Http.BadStatus code ->
                    -- text ("Could not get token. Status " ++ String.fromInt code)
                    text
                        ("Could not get token."
                            ++ (case code of
                                    401 ->
                                        "Wrong Username or Password."

                                    500 ->
                                        "Server Error."

                                    _ ->
                                        "Status: " ++ String.fromInt code
                               )
                        )

                _ ->
                    text ("Could not get token. Error occured. :: " ++ Debug.toString error)

        Nothing ->
            text "a nondescript error occured"
    ]


viewAttributes : User -> ViewState -> Html Msg
viewAttributes user viewState =
    div []
        [ text "Attributes: "
        , user.attributes
            |> Array.indexedMap
                (\index att -> lazy3 viewAttribute index att viewState)
            |> Array.toList
            |> ul []
            -- |> lazy
            |> List.singleton
            |> div []

        -- |> viewTextListToUl
        ]


viewAttribute : Int -> Attribute -> ViewState -> Html Msg
viewAttribute index att viewState =
    let
        patching =
            viewState.loading /= Nothing

        key =
            AttributeKey index att
    in
    li []
        [ div
            [ onMouseOver <| HoverEvent key True
            , onMouseOut <| HoverEvent key False
            ]
            [ let
                attHtml =
                    [ text att.short, text " - ", text att.name ]

                ( editingThis, untouched ) =
                    case viewState.editing of
                        Just (AttributeKey index_ att_) ->
                            if index == index_ && att == att_ then
                                ( Just att_, True )

                            else if index == index_ then
                                ( Just att_, False )

                            else
                                ( Nothing, False )

                        _ ->
                            ( Nothing, False )
              in
              case editingThis of
                Just att_ ->
                    let
                        ( nameinput, descinput ) =
                            ( input
                                [ onInput (EditingAttributePart (AName index))
                                , value att_.name
                                ]
                                []
                            , case att_.description of
                                Just desc ->
                                    input
                                        [ onInput
                                            (EditingAttributePart (ADescription index))
                                        , value desc
                                        ]
                                        []

                                Nothing ->
                                    input
                                        [ onInput
                                            (EditingAttributePart (ADescription index))
                                        ]
                                        []
                            )
                    in
                    div []
                        [ --text "untouched editing"
                          nameinput
                        , if untouched then
                            text ""

                          else
                            button [ onClick (ClickDiscardEdit key) ] [ text "X" ]
                        , button [ onClick <| ClickExpand key True ] [ text "⇧" ]
                        , div [] [ descinput ]
                        ]

                Nothing ->
                    if viewState.hovering == Just (AttributeKey index att) then
                        div []
                            (attHtml
                                ++ [ button [ onClick <| ClickExpand key False ] [ text "⇩" ]
                                   ]
                            )

                    else
                        div [] attHtml
            ]
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
            patchState.state == UserLoadingStateWait

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
