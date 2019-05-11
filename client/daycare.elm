module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, form, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Extra as Html
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Http
import Json
import Json.Decode as D
import Json.Encode as E
import Model exposing (..)
import Planner


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init () =
    ( { loginState =
            { username = "fefe"
            , password = "123456"
            , state = LoadingStateIdle
            }
      , userLoadState = LoadingStateIdle
      , dayLoadState = LoadingStateIdle
      , viewPatchState =
            { state = LoadingStateIdle
            , editing = Nothing
            }
      , errormsg = Nothing
      }
    , Cmd.none
    )


type Msg
    = UserLoginLoad
    | UserLoginLoaded (Result Http.Error String)
    | UserLoginLoadedLoadUserContent (Result Http.Error String)
    | UserContentLoad
    | UserContentLoaded (Result Http.Error User)
    | ViewMessages ViewMessage
    | UserPartLoaded UserPart
    | DaysLoad
    | DaysLoaded (Result Http.Error Days)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserLoginLoad ->
            ( setLoginStateLoadingstate model LoadingStateWait, login model.loginState )

        UserLoginLoaded result ->
            case result of
                Ok token ->
                    ( setLoginStateLoadingstate model (LoginStateSuccess token), Cmd.none )

                Err message ->
                    ( setLoginStateLoadingstate model (LoadingStateError <| Just message), Cmd.none )

        UserLoginLoadedLoadUserContent result ->
            case result of
                Ok token ->
                    let
                        os =
                            model.loginState

                        ls =
                            { os | state = LoginStateSuccess token }
                    in
                    ( { model | loginState = ls, userLoadState = LoadingStateWait }, loadUser ls )

                Err message ->
                    ( setLoginStateLoadingstate model (LoadingStateError <| Just message), Cmd.none )

        UserContentLoad ->
            ( { model | userLoadState = LoadingStateWait }, loadUser model.loginState )

        UserContentLoaded result ->
            case result of
                Ok content ->
                    ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent content) }, Cmd.none )

                Err message ->
                    ( { model | userLoadState = LoadingStateError <| Just message }, Cmd.none )

        ViewMessages (UpdateUsername u) ->
            ( setLoginStateUsername model u, Cmd.none )

        ViewMessages (UpdatePassword p) ->
            ( setLoginStatePassword model p, Cmd.none )

        ViewMessages (UpdateAttribute (AShort index value)) ->
            updateAttributePart model index value (AShort index value)

        ViewMessages (UpdateAttribute (AName index value)) ->
            updateAttributePart model index value (AName index value)

        ViewMessages (UpdateAttribute (ADescription index value)) ->
            updateAttributePart model index value (ADescription index value)

        ViewMessages (UpdateAttribute (ASend index)) ->
            updateAttributeSend model index

        ViewMessages (UpdateGoal (GName index value)) ->
            updateGoalPart model index value (GName index value)

        ViewMessages (UpdateGoal (GDescription index value)) ->
            updateGoalPart model index value (GDescription index value)

        ViewMessages (UpdateGoal (GSend index)) ->
            updateGoalSend model index

        ViewMessages (UpdateDay part) ->
            updateDayPart model part

        UserPartLoaded part ->
            -- (LoadedAttribute result) ->
            let
                up =
                    { state = LoadingStateSuccess Nothing
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
                ( { model | viewPatchState = up }, loadUser model.loginState )

            else
                ( { model | viewPatchState = { editing = Nothing, state = LoadingStateError Nothing } }, Cmd.none )

        DaysLoad ->
            ( { model | dayLoadState = LoadingStateWait }, loadDays model.loginState )

        -- ( model, Cmd.none )
        DaysLoaded result ->
            case result of
                Ok days ->
                    ( { model | dayLoadState = LoadingStateSuccess <| Just <| DaysContent days }, Cmd.none )

                Err message ->
                    ( { model | dayLoadState = LoadingStateError <| Just message }, Cmd.none )



-- ViewMessages (TestMessage index string) ->
--     ( { model | errormsg = Just ("Element with index " ++ String.fromInt index ++ " has value " ++ string) }, Cmd.none )


updateAttributePart : Model -> Int -> String -> AttributeKey -> ( Model, Cmd Msg )
updateAttributePart model index value attPart =
    case model.userLoadState of
        LoadingStateSuccess maybecontent ->
            case maybecontent of
                Just (UserContent content) ->
                    let
                        att =
                            Array.get index content.attributes

                        eCont =
                            case att of
                                Just att_ ->
                                    case attPart of
                                        AShort _ _ ->
                                            { content | attributes = Array.set index { att_ | short = value } content.attributes }

                                        AName _ _ ->
                                            { content | attributes = Array.set index { att_ | name = value } content.attributes }

                                        ADescription _ _ ->
                                            { content | attributes = Array.set index { att_ | description = Just value } content.attributes }

                                        _ ->
                                            content

                                Nothing ->
                                    content

                        up =
                            { state = model.viewPatchState.state
                            , editing =
                                case att of
                                    Just att_ ->
                                        Just (AEditState index att_)

                                    Nothing ->
                                        Nothing
                            }
                    in
                    ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent eCont), viewPatchState = up }, Cmd.none )

                _ ->
                    ( { model | userLoadState = LoadingStateError Nothing }, Cmd.none )

        _ ->
            ( { model | userLoadState = LoadingStateError <| Just Http.NetworkError }, Cmd.none )


updateAttributeSend : Model -> Int -> ( Model, Cmd Msg )
updateAttributeSend model index =
    case model.userLoadState of
        LoadingStateSuccess successcontent ->
            let
                up =
                    { state = LoadingStateWait
                    , editing = model.viewPatchState.editing
                    }

                upE =
                    { up | state = LoadingStateError Nothing }

                responseError =
                    ( { model | viewPatchState = upE }, Cmd.none )
            in
            case successcontent of
                Just (UserContent content) ->
                    let
                        att =
                            Array.get index content.attributes
                    in
                    case att of
                        Just value ->
                            case model.loginState.state of
                                LoginStateSuccess token ->
                                    ( { model | viewPatchState = up }, updateAttributes token value )

                                -- ( { model | userLoadState = LoadingStateWait, viewPatchState = up }, updateAttributes token value )
                                _ ->
                                    responseError

                        Nothing ->
                            responseError

                _ ->
                    ( { model | viewPatchState = up }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateGoalPart : Model -> Int -> String -> GoalKey -> ( Model, Cmd Msg )
updateGoalPart model index value goalPart =
    case model.userLoadState of
        LoadingStateSuccess maybecontent ->
            case maybecontent of
                Just (UserContent content) ->
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
                            { state = model.viewPatchState.state
                            , editing =
                                case maybegoal of
                                    Just goal_ ->
                                        Just (GEditState index goal_)

                                    Nothing ->
                                        Nothing
                            }
                    in
                    ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent eCont), viewPatchState = up }, Cmd.none )

                _ ->
                    ( { model | userLoadState = LoadingStateError Nothing }, Cmd.none )

        _ ->
            ( { model | userLoadState = LoadingStateError <| Just Http.NetworkError }, Cmd.none )


updateGoalSend : Model -> Int -> ( Model, Cmd Msg )
updateGoalSend model index =
    case model.userLoadState of
        LoadingStateSuccess successcontent ->
            let
                up =
                    { state = LoadingStateWait
                    , editing = model.viewPatchState.editing
                    }

                upE =
                    { up | state = LoadingStateError Nothing }

                responseError =
                    ( { model | viewPatchState = upE }, Cmd.none )
            in
            case successcontent of
                Just (UserContent content) ->
                    let
                        att =
                            Array.get index content.goals
                    in
                    case att of
                        Just value ->
                            case model.loginState.state of
                                LoginStateSuccess token ->
                                    ( { model | viewPatchState = up }, updateGoals token value )

                                -- ( { model | userLoadState = LoadingStateWait, viewPatchState = up }, updateAttributes token value )
                                _ ->
                                    responseError

                        Nothing ->
                            responseError

                _ ->
                    ( { model | viewPatchState = up }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateDayPart : Model -> DayKey -> ( Model, Cmd Msg )
updateDayPart model dayPart =
    let
        vps =
            model.viewPatchState

        errReturn =
            { model
                | viewPatchState =
                    { editing = Nothing
                    , state = LoadingStateError Nothing
                    }
            }
    in
    case model.userLoadState of
        LoadingStateSuccess maybecontent ->
            case maybecontent of
                Just (DaysContent days) ->
                    let
                        -- ( key, index, value ) =
                        --     case dayPart of
                        --         DDate i v ->
                        --             ( "date", i, v )
                        --         DDescription i v ->
                        --             ( i, v )
                        h =
                            ""

                        -- maybeday =
                        --     Array.get index days
                        -- -- TODO: either get this to work this way around
                        -- -- or just copy structure of updateGoalPart :/
                        -- Array.set index
                        -- content = case maybeday of
                        --     Nothing ->
                        --         { day |  }
                    in
                    ( model, Cmd.none )

                -- Just day ->
                --     ( { model | userLoadState = LoadingStateSuccess <| Just day }, Cmd.none )
                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- let
--     maybeday = Array.get index days
--     eCont =
--         case maybeday of
--             Just day ->
--                 case dayPart of
--                     DDate _ _ ->
--                         {}


setLoginStateUsername : Model -> String -> Model
setLoginStateUsername model u =
    let
        oldVs =
            model.loginState

        newVs =
            { oldVs | username = u }
    in
    { model | loginState = newVs }


setLoginStatePassword : Model -> String -> Model
setLoginStatePassword model p =
    let
        oldVs =
            model.loginState

        newVs =
            { oldVs | password = p }
    in
    { model | loginState = newVs }


setLoginStateLoadingstate : Model -> LoadState -> Model
setLoginStateLoadingstate model state =
    let
        oldVs =
            model.loginState

        newVs =
            { oldVs | state = state }
    in
    { model | loginState = newVs }


updateAttributes : String -> Attribute -> Cmd Msg
updateAttributes token attribute =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| Json.encodeAttribute attribute
        , headers = [ Http.header "token" token ]
        , url = "http://localhost:5000/api/v3/users/me/attributes/" ++ attribute.short_
        , expect = Http.expectJson (UserPartLoaded << LoadedAttribute) Json.decodeAttribute
        }


updateGoals : String -> Goal -> Cmd Msg
updateGoals token goal =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| Json.encodeGoal goal
        , headers = [ Http.header "token" token ]
        , url = "http://localhost:5000/api/v3/users/me/goals/" ++ goal.name_
        , expect = Http.expectJson (UserPartLoaded << LoadedGoal) Json.decodeGoal
        }



-- setLoadUserStateLoadingState : Model -> LoadState -> Model
-- setLoadUserStateLoadingState model state =
--     let
--         oldVs =
--             model.userLoadState
--         newVs =
--             { oldVs | state = state }
--     in
--     { model | userLoadState = newVs }


login : ViewLoginState -> Cmd Msg
login loginState =
    Http.post
        { url = "http://localhost:5000/api/v3/login"
        , body = Http.jsonBody (Json.encodeUserLogin loginState)
        , expect = Http.expectJson UserLoginLoadedLoadUserContent Json.decodeUserLogin
        }


loadUser : ViewLoginState -> Cmd Msg
loadUser loginState =
    case loginState.state of
        LoginStateSuccess token ->
            Http.request
                { method = "GET"
                , timeout = Nothing
                , tracker = Nothing
                , body = Http.emptyBody
                , headers = [ Http.header "token" token ]
                , url = "http://localhost:5000/api/v3/users/me"
                , expect = Http.expectJson UserContentLoaded Json.decodeUserLoadState
                }

        _ ->
            Debug.todo "?? requesting loading user without having logged in lul"


loadDays loginState =
    case loginState.state of
        LoginStateSuccess token ->
            Http.request
                { method = "GET"
                , timeout = Nothing
                , tracker = Nothing
                , body = Http.emptyBody
                , headers = [ Http.header "token" token ]
                , url = "http://localhost:5000/api/v3/users/me/days"
                , expect = Http.expectJson DaysLoaded Json.decodeDays
                }

        _ ->
            Debug.todo "trying to load days without logging in"


view : Model -> Html Msg
view model =
    div []
        [ div []
            (case model.loginState.state of
                LoginStateSuccess token ->
                    case model.userLoadState of
                        LoadingStateSuccess successcontent ->
                            case successcontent of
                                Just (UserContent user) ->
                                    let
                                        editing =
                                            model.viewPatchState.editing
                                    in
                                    [ div []
                                        [ text ("email: " ++ user.email) ]
                                    , viewAttributes user model.viewPatchState
                                    , div [] []
                                    , viewGoals user model.viewPatchState
                                    , div [] []
                                    , viewDays model.dayLoadState model.viewPatchState
                                    ]

                                _ ->
                                    [ text "An error occured. No Usercontent was loaded" ]

                        LoadingStateError error ->
                            [ div []
                                (viewResponseHttpError error)
                            , div []
                                [ text ("Logged in! Token: " ++ token)
                                , div []
                                    [ button [ onClick UserContentLoad ] [ text "Load Content" ]
                                    ]
                                ]
                            ]

                        LoadingStateWait ->
                            [ text "Loading content..." ]

                        LoadingStateIdle ->
                            [ text ("Logged in! Token: " ++ token)
                            , div []
                                [ button [ onClick UserContentLoad ] [ text "Load Content" ]
                                ]
                            ]

                        _ ->
                            viewUnknownError

                LoadingStateIdle ->
                    viewLoginForm model.loginState (text "Login to get token.")

                LoadingStateWait ->
                    viewLoginForm model.loginState (text "Logging in...")

                LoadingStateError error ->
                    viewResponseHttpError error

                _ ->
                    viewUnknownError
            )
        ]



-- viewLoginForm : ViewLoginState -> Html Msg -> List (Html Msg)


viewUnknownError =
    [ div [] [ text "An unknown Error occurred" ] ]


viewLoginForm loginState appendix =
    [ div []
        [ text "Username:"
        , input [ placeholder "username", value loginState.username, onInput (ViewMessages << UpdateUsername) ] []
        ]
    , div []
        [ text "Password:"
        , input [ placeholder "Password", value loginState.password, onInput (ViewMessages << UpdatePassword) ] []
        ]
    , div []
        [ button [ onClick UserLoginLoad ] [ text "Login" ] ]
    , div [] [ appendix ]
    ]


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


viewAttributes : User -> ViewPatchState -> Html Msg
viewAttributes user viewPatchState =
    div []
        [ text "Attributes: "
        , user.attributes
            |> Array.indexedMap
                (\index att -> lazy3 viewAttribute index att viewPatchState)
            |> Array.toList
            |> ul []
            -- |> lazy
            |> List.singleton
            |> div []

        -- |> viewTextListToUl
        ]


viewAttribute : Int -> Attribute -> ViewPatchState -> Html Msg
viewAttribute index att patchState =
    let
        patching =
            patchState.state == LoadingStateWait
    in
    li []
        [ div []
            [ Html.form
                [ onSubmit <| ViewMessages <| UpdateAttribute <| ASend index ]
                [ input
                    [ value att.short
                    , onInput (ViewMessages << UpdateAttribute << AShort index)
                    , disabled patching
                    , alt "Short"
                    ]
                    []
                , input
                    [ value att.name
                    , onInput (ViewMessages << UpdateAttribute << AName index)
                    , disabled patching
                    , alt "Name"
                    ]
                    []
                , case att.description of
                    Just desc ->
                        input
                            [ value desc
                            , onInput (ViewMessages << UpdateAttribute << ADescription index)
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


viewGoals : User -> ViewPatchState -> Html Msg
viewGoals user viewPatchState =
    div []
        [ text "Goals: "
        , user.goals
            |> Array.indexedMap
                (\index goal -> viewGoal index goal viewPatchState)
            |> Array.toList
            |> ul []
            |> List.singleton
            |> div []

        -- |> viewTextListToUl
        ]


viewGoal : Int -> Goal -> ViewPatchState -> Html Msg
viewGoal index goal patchState =
    let
        patching =
            patchState.state == LoadingStateWait

        editing =
            patchState.editing
    in
    -- [ Html.form [ onSubmit <| ViewMessages <| TestMessage index "u" ]
    li []
        [ div []
            [ Html.form
                [ onSubmit <| ViewMessages <| UpdateGoal <| GSend index ]
                [ input
                    [ value goal.name
                    , onInput (ViewMessages << UpdateGoal << GName index)
                    , disabled patching
                    , alt "Name"
                    ]
                    []
                , case goal.description of
                    Just desc ->
                        input
                            [ value desc
                            , onInput (ViewMessages << UpdateGoal << GDescription index)
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


viewDays : LoadState -> ViewPatchState -> Html Msg
viewDays dayLoadState patchState =
    div []
        [ let
            ( btntext, btnwait ) =
                case dayLoadState of
                    LoadingStateIdle ->
                        ( "Load days", False )

                    LoadingStateWait ->
                        ( "Loading...", True )

                    _ ->
                        ( "Reload days", False )
          in
          button [ onClick DaysLoad, disabled btnwait ] [ text btntext ]
        , case dayLoadState of
            LoadingStateError err ->
                div []
                    [ text "An Error occurred while fetching days."
                    , text <| " error: " ++ Debug.toString err
                    ]

            LoadingStateSuccess successcontent ->
                case successcontent of
                    Just (DaysContent content) ->
                        div []
                            [ text "Days:"
                            , content
                                |> Array.map
                                    (\day -> lazy2 viewDay day patchState)
                                -- (\_ -> text <| Debug.toString)
                                |> Array.toList
                                |> ul []
                                |> List.singleton
                                |> div []
                            ]

                    _ ->
                        text "There was a weird Error. Loaded successfully but received the wrong content."

            _ ->
                Html.nothing
        ]


viewDay : Day -> ViewPatchState -> Html Msg
viewDay day patchState =
    let
        patching =
            patchState.state == LoadingStateWait
    in
    li []
        [ div []
            [ Html.form
                []
                [ input
                    [ value day.date
                    , disabled patching
                    , alt "Date"
                    ]
                    []
                , case day.description of
                    Just desc ->
                        input
                            [ value desc
                            , disabled patching
                            , alt "Description"
                            ]
                            []

                    Nothing ->
                        Html.nothing
                , case day.attributes of
                    Just attributes ->
                        attributes
                            |> List.map
                                (\att ->
                                    att
                                        |> text
                                        |> List.singleton
                                        |> li []
                                 -- |> List.singleton
                                )
                            |> div []
                            |> List.singleton
                            |> ul []

                    Nothing ->
                        Html.nothing
                , case patchState.editing of
                    Just (DEditState day_) ->
                        if day_.date_ == day.date then
                            if patching then
                                text "(...)"

                            else
                                input [ value "Ok", type_ "submit" ] []

                        else
                            Html.nothing

                    _ ->
                        Html.nothing
                ]
            ]
        ]
