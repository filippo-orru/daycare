module Main exposing (main)

import Array exposing (Array)
import Browser
import Decoders exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Extra as Html
import Http
import Json.Decode as D
import Json.Encode as E



-- import Model exposing (..)
-- import Views exposing (..)


type alias Model =
    { loginState : ViewLoginState
    , userLoadState : LoadState
    , userPatchState : ViewPatchState
    , errormsg : Maybe String

    -- , editState : EditState
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { loginState =
            { username = "fefe"
            , password = "123456"
            , state = LoadingStateIdle
            }
      , userLoadState = LoadingStateIdle
      , userPatchState =
            { state = LoadingStateIdle
            , editing = Nothing
            }

      --   , editState =
      --         { goalEditState = Nothing
      --         , attributeEditState = Nothing
      --         }
      , errormsg = Nothing
      }
    , Cmd.none
    )


type Msg
    = LoadUserLogin
    | LoadUserContent
    | UserLoginLoaded (Result Http.Error String)
    | UserContentLoaded (Result Http.Error UserContent)
    | ViewMessages ViewMessage
    | LoadedPart LoadedPart


type alias ViewLoginState =
    { username : String
    , password : String
    , state : LoadState
    }


type alias ViewPatchState =
    { state : LoadState
    , editing : Maybe EditState
    }


type alias UserContent =
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
    = GEditState_ Int Goal
    | AEditState_ Int Attribute



-- type KeyTypes
--     = KeyGoal
--     | KeyAttribute


type LoadState
    = LoadingStateIdle
    | LoadingStateWait
    | LoadingStateError (Maybe Http.Error)
    | LoadingStateSuccess (Maybe SuccessContent)
    | LoginStateSuccess String



-- | LoginStateSuccess String


type SuccessContent
    = UserContent_ UserContent



-- | String_ String


type LoadedPart
    = LoadedAttribute (Result Http.Error Attribute)
    | LoadedGoal (Result Http.Error Goal)


type ViewMessage
    = UpdateUsername String
    | UpdatePassword String
    | UpdateAttribute AttributeKey
    | UpdateGoal GoalKey



-- | TestMessage Int String


type AttributeKey
    = AShort Int String
    | AName Int String
    | ADescription Int String
    | ASend Int


type GoalKey
    = GName Int String
    | GDescription Int String
    | GSend Int


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadUserLogin ->
            ( setLoginStateLoadingstate model LoadingStateWait, login model.loginState )

        LoadUserContent ->
            ( { model | userLoadState = LoadingStateWait }, loadUser model.loginState )

        UserLoginLoaded result ->
            case result of
                Ok token ->
                    ( setLoginStateLoadingstate model (LoginStateSuccess token), Cmd.none )

                Err message ->
                    ( setLoginStateLoadingstate model (LoadingStateError <| Just message), Cmd.none )

        UserContentLoaded result ->
            case result of
                Ok content ->
                    ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent_ content) }, Cmd.none )

                Err message ->
                    ( { model | userLoadState = LoadingStateError <| Just message }, Cmd.none )

        ViewMessages (UpdateUsername u) ->
            ( setLoginStateUsername model u, Cmd.none )

        ViewMessages (UpdatePassword p) ->
            ( setLoginStatePassword model p, Cmd.none )

        ViewMessages (UpdateAttribute (AShort index value)) ->
            case model.userLoadState of
                LoadingStateSuccess successcontent ->
                    case successcontent of
                        Just (UserContent_ content) ->
                            let
                                att =
                                    Array.get index content.attributes

                                eCont =
                                    case att of
                                        Just att_ ->
                                            { content | attributes = Array.set index { att_ | short = value } content.attributes }

                                        Nothing ->
                                            content

                                up =
                                    { state = model.userPatchState.state
                                    , editing =
                                        case att of
                                            Just att_ ->
                                                Just (AEditState_ index att_)

                                            Nothing ->
                                                Nothing
                                    }
                            in
                            ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent_ eCont), userPatchState = up }, Cmd.none )

                        _ ->
                            ( { model | userLoadState = LoadingStateError Nothing }, Cmd.none )

                _ ->
                    ( { model | userLoadState = LoadingStateError <| Just Http.NetworkError }, Cmd.none )

        ViewMessages (UpdateAttribute (AName index value)) ->
            updateAttributePart model index value (AName index value)

        ViewMessages (UpdateAttribute (ADescription index value)) ->
            updateAttributePart model index value (ADescription index value)

        ViewMessages (UpdateAttribute (ASend index)) ->
            updateAttributeSend model index

        ViewMessages (UpdateGoal (GName index value)) ->
            updateGoalPart model index value (GName index value)

        ViewMessages (UpdateGoal (GDescription index value)) ->
            updateGoalPart model index value (GName index value)

        ViewMessages (UpdateGoal (GSend index)) ->
            updateGoalSend model index

        LoadedPart part ->
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
                ( { model | userPatchState = up }, loadUser model.loginState )

            else
                ( { model | userPatchState = { editing = Nothing, state = LoadingStateError Nothing } }, Cmd.none )



-- ViewMessages (TestMessage index string) ->
--     ( { model | errormsg = Just ("Element with index " ++ String.fromInt index ++ " has value " ++ string) }, Cmd.none )


updateAttributePart : Model -> Int -> String -> AttributeKey -> ( Model, Cmd Msg )
updateAttributePart model index value attPart =
    case model.userLoadState of
        LoadingStateSuccess maybecontent ->
            case maybecontent of
                Just (UserContent_ content) ->
                    let
                        att =
                            Array.get index content.attributes

                        eCont =
                            case att of
                                Just att_ ->
                                    case attPart of
                                        AShort _ _ ->
                                            { content | attributes = Array.set index { att_ | name = value } content.attributes }

                                        AName _ _ ->
                                            { content | attributes = Array.set index { att_ | name = value } content.attributes }

                                        ADescription _ _ ->
                                            { content | attributes = Array.set index { att_ | description = Just value } content.attributes }

                                        _ ->
                                            content

                                Nothing ->
                                    content

                        up =
                            { state = model.userPatchState.state
                            , editing =
                                case att of
                                    Just att_ ->
                                        Just (AEditState_ index att_)

                                    Nothing ->
                                        Nothing
                            }
                    in
                    ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent_ eCont), userPatchState = up }, Cmd.none )

                _ ->
                    ( { model | userLoadState = LoadingStateError Nothing }, Cmd.none )

        _ ->
            ( { model | userLoadState = LoadingStateError <| Just Http.NetworkError }, Cmd.none )


updateAttributeSend : Model -> Int -> ( Model, Cmd Msg )
updateAttributeSend model index =
    let
        --
        h =
            ""
    in
    case model.userLoadState of
        LoadingStateSuccess successcontent ->
            let
                up =
                    { state = LoadingStateWait
                    , editing = model.userPatchState.editing
                    }

                upE =
                    { up | state = LoadingStateError Nothing }

                responseError =
                    ( { model | userPatchState = upE }, Cmd.none )
            in
            case successcontent of
                Just (UserContent_ content) ->
                    let
                        att =
                            Array.get index content.attributes
                    in
                    case att of
                        Just value ->
                            case model.loginState.state of
                                LoginStateSuccess token ->
                                    ( { model | userPatchState = up }, updateAttributes token value )

                                -- ( { model | userLoadState = LoadingStateWait, userPatchState = up }, updateAttributes token value )
                                _ ->
                                    responseError

                        Nothing ->
                            responseError

                _ ->
                    ( { model | userPatchState = up }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateGoalPart : Model -> Int -> String -> GoalKey -> ( Model, Cmd Msg )
updateGoalPart model index value goalPart =
    case model.userLoadState of
        LoadingStateSuccess maybecontent ->
            case maybecontent of
                Just (UserContent_ content) ->
                    let
                        goal =
                            Array.get index content.goals

                        eCont =
                            case goal of
                                Just goal_ ->
                                    case goalPart of
                                        GName _ _ ->
                                            { content | goals = Array.set index { goal_ | name = value } content.goals }

                                        GDescription _ _ ->
                                            { content | goals = Array.set index { goal_ | description = Just value } content.goals }

                                        _ ->
                                            content

                                Nothing ->
                                    content

                        up =
                            { state = model.userPatchState.state
                            , editing =
                                case goal of
                                    Just goal_ ->
                                        Just (GEditState_ index goal_)

                                    Nothing ->
                                        Nothing
                            }
                    in
                    ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent_ eCont), userPatchState = up }, Cmd.none )

                _ ->
                    ( { model | userLoadState = LoadingStateError Nothing }, Cmd.none )

        _ ->
            ( { model | userLoadState = LoadingStateError <| Just Http.NetworkError }, Cmd.none )


updateGoalSend : Model -> Int -> ( Model, Cmd Msg )
updateGoalSend model index =
    let
        --
        h =
            ""
    in
    case model.userLoadState of
        LoadingStateSuccess successcontent ->
            let
                up =
                    { state = LoadingStateWait
                    , editing = model.userPatchState.editing
                    }

                upE =
                    { up | state = LoadingStateError Nothing }

                responseError =
                    ( { model | userPatchState = upE }, Cmd.none )
            in
            case successcontent of
                Just (UserContent_ content) ->
                    let
                        att =
                            Array.get index content.goals
                    in
                    case att of
                        Just value ->
                            case model.loginState.state of
                                LoginStateSuccess token ->
                                    ( { model | userPatchState = up }, updateGoals token value )

                                -- ( { model | userLoadState = LoadingStateWait, userPatchState = up }, updateAttributes token value )
                                _ ->
                                    responseError

                        Nothing ->
                            responseError

                _ ->
                    ( { model | userPatchState = up }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
        , body = Http.jsonBody <| encodeAttribute attribute
        , headers = [ Http.header "token" token ]
        , url = "http://localhost:5000/api/v3/users/me/attributes/" ++ attribute.short_
        , expect = Http.expectJson (LoadedPart << LoadedAttribute) attributeDecoder
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
        , expect = Http.expectJson (LoadedPart << LoadedGoal) goalDecoder
        }


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



-- setLoadUserStateLoadingState : Model -> LoadState -> Model
-- setLoadUserStateLoadingState model state =
--     let
--         oldVs =
--             model.userLoadState
--         newVs =
--             { oldVs | state = state }
--     in
--     { model | userLoadState = newVs }


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
                , expect = Http.expectJson UserContentLoaded userLoadStateDecoder
                }

        _ ->
            Debug.todo "?? requesting loading user without having logged in lul"


login : ViewLoginState -> Cmd Msg
login loginState =
    Http.post
        { url = "http://localhost:5000/api/v3/login"
        , body = Http.jsonBody (userLoginEncoder loginState)
        , expect = Http.expectJson UserLoginLoaded userLoginDecoder
        }


userLoginEncoder : ViewLoginState -> E.Value
userLoginEncoder loginState =
    E.object
        [ ( "username", E.string loginState.username )
        , ( "password", E.string loginState.password )
        ]


userLoginDecoder : D.Decoder String
userLoginDecoder =
    D.field "token" D.string


userLoadStateDecoder : D.Decoder UserContent
userLoadStateDecoder =
    D.map3 UserContent
        (D.field "email" D.string)
        (D.field "attributes" (D.array attributeDecoder))
        (D.field "goals" (D.array goalDecoder))


attributeDecoder =
    D.map4 Attribute
        (D.field "short" D.string)
        (D.field "short" D.string)
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))


goalDecoder =
    D.map4 Goal
        (D.field "name" D.string)
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))
        (D.maybe (D.field "deadline" D.string))


view : Model -> Html Msg
view model =
    div []
        [ -- div []
          -- [ case model.errormsg of
          --     Just val ->
          --         text ("Error" ++ val)
          --     Nothing ->
          --         text "No error "
          -- ]
          div []
            (case model.loginState.state of
                LoginStateSuccess token ->
                    case model.userLoadState of
                        LoadingStateSuccess successcontent ->
                            case successcontent of
                                Just (UserContent_ user) ->
                                    let
                                        editing =
                                            model.userPatchState.editing
                                    in
                                    [ div []
                                        [ text ("email: " ++ user.email) ]
                                    , text
                                        (case editing of
                                            Just edst ->
                                                case edst of
                                                    AEditState_ index att_ ->
                                                        " ( editing index [" ++ String.fromInt index ++ "] with name " ++ att_.name ++ ")"

                                                    _ ->
                                                        Debug.todo "implement goal"

                                            Nothing ->
                                                " ( not editing )"
                                        )
                                    , viewAttributes user model.userPatchState
                                    , div [] []
                                    , viewGoals user model.userPatchState

                                    -- crepe?
                                    ]

                                _ ->
                                    [ text "An error occured. No Usercontent was loaded" ]

                        LoadingStateError error ->
                            [ div []
                                (viewResponseHttpError error)
                            , div []
                                [ text ("Logged in! Token: " ++ token)
                                , div []
                                    [ button [ onClick LoadUserContent ] [ text "Load Content" ]
                                    ]
                                ]
                            ]

                        LoadingStateWait ->
                            [ text "Loading..." ]

                        _ ->
                            [ text ("Logged in! Token: " ++ token)
                            , div []
                                [ button [ onClick LoadUserContent ] [ text "Load Content" ]
                                ]
                            ]

                LoadingStateIdle ->
                    viewLoginForm model.loginState (text "Login to get token.")

                LoadingStateWait ->
                    viewLoginForm model.loginState (text "Loading...")

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
        [ button [ onClick LoadUserLogin ] [ text "Login" ] ]
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


viewAttributes : UserContent -> ViewPatchState -> Html Msg
viewAttributes user userPatchState =
    div []
        [ text "Attributes: "
        , user.attributes
            |> Array.indexedMap
                (\index att -> viewAttribute index att userPatchState)
            |> Array.toList
            |> ul []
            |> List.singleton
            |> div []

        -- |> viewTextListToUl
        ]


viewAttribute : Int -> Attribute -> ViewPatchState -> Html Msg
viewAttribute index att patchState =
    let
        short =
            att.short

        name =
            att.name

        patching =
            patchState.state == LoadingStateWait
    in
    -- [ Html.form [ onSubmit <| ViewMessages <| TestMessage index "u" ]
    li []
        [ div []
            [ Html.form
                [ onSubmit <| ViewMessages <| UpdateAttribute <| ASend index ]
                [ input
                    [ value short
                    , onInput (ViewMessages << UpdateAttribute << AShort index)
                    , disabled patching
                    ]
                    []
                , input
                    [ value name
                    , onInput (ViewMessages << UpdateAttribute << AName index)
                    , disabled patching
                    ]
                    []
                , case att.description of
                    Just desc ->
                        input
                            [ value desc
                            , onInput (ViewMessages << UpdateAttribute << ADescription index)
                            , disabled patching
                            ]
                            []

                    Nothing ->
                        Html.nothing
                , case patchState.editing of
                    Just (AEditState_ index_ att_) ->
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


viewGoals : UserContent -> ViewPatchState -> Html Msg
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
                    ]
                    []
                , case goal.description of
                    Just desc ->
                        input
                            [ value desc
                            , onInput (ViewMessages << UpdateGoal << GDescription index)
                            , disabled patching
                            ]
                            []

                    Nothing ->
                        Html.nothing
                , case patchState.editing of
                    Just (GEditState_ index_ att_) ->
                        -- if editstate == AEditState_  then
                        if index_ == index then
                            if patching then
                                text "(...)"

                            else
                                input
                                    [ value "Ok", type_ "submit" ]
                                    []
                            -- else if patching then

                        else
                            Html.nothing

                    _ ->
                        --   else
                        Html.nothing
                ]
            ]
        ]
