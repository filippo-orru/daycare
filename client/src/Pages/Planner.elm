module Pages.Planner exposing (Attribute, AttributeKey(..), DayKey(..), EditState(..), Goal, GoalKey(..), Model, Msg(..), User, UserLoadState(..), UserPart(..), decodeAttribute, decodeGoal, decodeUserLoadState, encodeAttribute, encodeGoal, init, loadDays, loadUser, toSession, update, updateAttributePart, updateAttributeSend, updateAttributes, updateGoalPart, updateGoalSend, updateGoals, view, viewAttribute, viewAttributes, viewGoal, viewGoals, viewResponseHttpError)

import Array exposing (Array)
import Days exposing (Date, Day, Days, Task)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
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
    , userPatchState : UserPatchState

    -- , days : Days.Model
    , errormsg : Maybe String
    }


type UserLoadState
    = UserLoadingStateIdle
    | UserLoadingStateWait
    | UserLoadingStateError (Maybe Http.Error)
    | UserLoadingStateSuccess (Maybe User)


type alias UserPatchState =
    { state : UserLoadState
    , editing : Maybe EditState
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


type AttributeKey
    = AShort Int String
    | AName Int String
    | ADescription Int String
    | ASend Int


type GoalKey
    = GName Int String
    | GDescription Int String
    | GSend Int


type DayKey
    = DDate Int String
    | DDescription Int String


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , userLoadState = UserLoadingStateIdle
            , userPatchState =
                { state = UserLoadingStateIdle
                , editing = Nothing
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
    | UpdateAttribute AttributeKey
    | UpdateGoal GoalKey
    | UpdateDay DayKey
    | LoadedUserPart UserPart
    | LoadDays
    | LoadedDays (Result Http.Error Days)


update msg model =
    let
        maybetoken =
            Session.cred model.session
    in
    case maybetoken of
        Nothing ->
            ( model, Cmd.none )

        Just token ->
            case ( msg, model ) of
                ( LoadUserContent, _ ) ->
                    ( { model | userLoadState = UserLoadingStateWait }, loadUser token )

                ( LoadedUserContent (Ok content), _ ) ->
                    ( { model | userLoadState = UserLoadingStateSuccess (Just content) }, Cmd.none )

                ( LoadedUserContent (Err message), _ ) ->
                    ( { model | userLoadState = UserLoadingStateError <| Just message }, Cmd.none )

                ( UpdateAttribute (AShort index value), _ ) ->
                    updateAttributePart model index value (AShort index value)

                ( UpdateAttribute (AName index value), _ ) ->
                    updateAttributePart model index value (AName index value)

                ( UpdateAttribute (ADescription index value), _ ) ->
                    updateAttributePart model index value (ADescription index value)

                ( UpdateAttribute (ASend index), _ ) ->
                    updateAttributeSend model token index

                ( UpdateGoal (GName index value), _ ) ->
                    updateGoalPart model index value (GName index value)

                ( UpdateGoal (GDescription index value), _ ) ->
                    updateGoalPart model index value (GDescription index value)

                ( UpdateGoal (GSend index), _ ) ->
                    updateGoalSend model token index

                -- ( UpdateDay part, _ ) ->
                --     Days.updatePart model part
                ( LoadedUserPart part, _ ) ->
                    -- (LoadedAttribute result) ->
                    let
                        up =
                            { state = UserLoadingStateSuccess Nothing
                            , editing = Nothing
                            }

                        successful =
                            case part of
                                LoadedAttribute (Ok _) ->
                                    True

                                LoadedGoal (Ok _) ->
                                    True

                                _ ->
                                    False
                    in
                    if successful then
                        ( { model | userPatchState = up }, loadUser token )

                    else
                        ( { model | userPatchState = { editing = Nothing, state = UserLoadingStateError Nothing } }, Cmd.none )

                {- ( LoadDays, _ ) ->
                       ( { model | dayLoadState = Days.LoadingStateWait }, loadDays token )

                   -- ( model, Cmd.none )
                   ( LoadedDays result, _ ) ->
                       case result of
                           Ok days ->
                               ( { model | dayLoadState = Days.LoadingStateSuccess <| Just days }, Cmd.none )

                           Err message ->
                               ( { model | dayLoadState = Days.LoadingStateError <| Just message }, Cmd.none )
                -}
                ( _, _ ) ->
                    ( model, Cmd.none )


updateAttributePart : Model -> Int -> String -> AttributeKey -> ( Model, Cmd Msg )
updateAttributePart model index value attPart =
    case model.userLoadState of
        UserLoadingStateSuccess (Just content) ->
            let
                eCont =
                    case Array.get index content.attributes of
                        Just att ->
                            case attPart of
                                AShort _ _ ->
                                    { content | attributes = Array.set index { att | short = value } content.attributes }

                                AName _ _ ->
                                    { content | attributes = Array.set index { att | name = value } content.attributes }

                                ADescription _ _ ->
                                    { content | attributes = Array.set index { att | description = Just value } content.attributes }

                                _ ->
                                    content

                        Nothing ->
                            content

                up =
                    { state = model.userPatchState.state
                    , editing =
                        case Array.get index content.attributes of
                            Just att ->
                                Just (AEditState index att)

                            Nothing ->
                                Nothing
                    }
            in
            ( { model | userLoadState = UserLoadingStateSuccess (Just eCont), userPatchState = up }, Cmd.none )

        _ ->
            ( { model | userLoadState = UserLoadingStateError <| Just Http.NetworkError }, Cmd.none )


updateAttributeSend : Model -> String -> Int -> ( Model, Cmd Msg )
updateAttributeSend model token index =
    let
        up =
            { state = UserLoadingStateWait
            , editing = model.userPatchState.editing
            }

        upE =
            { up | state = UserLoadingStateError Nothing }

        responseError =
            ( { model | userPatchState = upE }, Cmd.none )
    in
    case model.userLoadState of
        UserLoadingStateSuccess (Just content) ->
            let
                att =
                    Array.get index content.attributes
            in
            case att of
                Just value ->
                    ( { model | userPatchState = up }, updateAttributes token value )

                Nothing ->
                    responseError

        _ ->
            ( model, Cmd.none )


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
                                GName _ _ ->
                                    { content | goals = Array.set index { goal | name = value } content.goals }

                                GDescription _ _ ->
                                    { content | goals = Array.set index { goal | description = Just value } content.goals }

                                _ ->
                                    content

                        Nothing ->
                            content

                up =
                    { state = model.userPatchState.state
                    , editing =
                        case maybegoal of
                            Just goal_ ->
                                Just (GEditState index goal_)

                            Nothing ->
                                Nothing
                    }
            in
            ( { model | userLoadState = UserLoadingStateSuccess (Just eCont), userPatchState = up }, Cmd.none )

        _ ->
            ( { model | userLoadState = UserLoadingStateError <| Just Http.NetworkError }, Cmd.none )


updateGoalSend : Model -> String -> Int -> ( Model, Cmd Msg )
updateGoalSend model token index =
    let
        up =
            { state = UserLoadingStateWait
            , editing = model.userPatchState.editing
            }

        upE =
            { up | state = UserLoadingStateError Nothing }

        responseError =
            ( { model | userPatchState = upE }, Cmd.none )
    in
    case model.userLoadState of
        UserLoadingStateSuccess (Just content) ->
            let
                att =
                    Array.get index content.goals
            in
            case att of
                Just value ->
                    ( { model | userPatchState = up }, updateGoals token value )

                Nothing ->
                    responseError

        _ ->
            ( model, Cmd.none )


updateAttributes : String -> Attribute -> Cmd Msg
updateAttributes token attribute =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeAttribute attribute
        , headers = [ Http.header "token" token ]
        , url = "http://localhost:5000/api/v3/users/me/attributes/" ++ attribute.short_
        , expect = Http.expectJson (LoadedUserPart << LoadedAttribute) decodeAttribute
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
        , expect = Http.expectJson (LoadedUserPart << LoadedGoal) decodeGoal
        }



-- View


view : Model -> List (Html Msg)
view model =
    case model.userLoadState of
        UserLoadingStateSuccess (Just user) ->
            let
                editing =
                    model.userPatchState.editing
            in
            [ div []
                [ text ("email: " ++ user.email) ]
            , viewAttributes user model.userPatchState
            , div [] []
            , viewGoals user model.userPatchState
            , div [] []

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


viewAttributes : User -> UserPatchState -> Html Msg
viewAttributes user userPatchState =
    div []
        [ text "Attributes: "
        , user.attributes
            |> Array.indexedMap
                (\index att -> lazy3 viewAttribute index att userPatchState)
            |> Array.toList
            |> ul []
            -- |> lazy
            |> List.singleton
            |> div []

        -- |> viewTextListToUl
        ]


viewAttribute : Int -> Attribute -> UserPatchState -> Html Msg
viewAttribute index att patchState =
    let
        patching =
            patchState.state == UserLoadingStateWait
    in
    li []
        [ div []
            [ Html.form
                [ onSubmit <| UpdateAttribute <| ASend index ]
                [ input
                    [ value att.short
                    , onInput (UpdateAttribute << AShort index)
                    , disabled patching
                    , alt "Short"
                    ]
                    []
                , input
                    [ value att.name
                    , onInput (UpdateAttribute << AName index)
                    , disabled patching
                    , alt "Name"
                    ]
                    []
                , case att.description of
                    Just desc ->
                        input
                            [ value desc
                            , onInput (UpdateAttribute << ADescription index)
                            , disabled patching
                            , alt "Description"
                            ]
                            []

                    Nothing ->
                        Html.nothing
                , case patchState.editing of
                    Just (AEditState index_ att_) ->
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
                        --   else
                        Html.nothing
                ]
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
            [ Html.form
                [ onSubmit (UpdateGoal <| GSend index) ]
                [ input
                    [ value goal.name
                    , onInput (UpdateGoal << GName index)
                    , disabled patching
                    , alt "Name"
                    ]
                    []
                , case goal.description of
                    Just desc ->
                        input
                            [ value desc
                            , onInput (UpdateGoal << GDescription index)
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


decodeGoal : D.Decoder Goal
decodeGoal =
    D.map4 Goal
        (D.field "name" D.string)
        (D.field "name" D.string)
        (D.maybe <| D.field "description" D.string)
        (D.maybe <| D.field "deadline" D.string)


toSession : Model -> Session
toSession model =
    model.session
