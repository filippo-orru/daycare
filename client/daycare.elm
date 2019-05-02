module Main exposing (main)

import Array exposing (Array)
import Browser
import Decoders exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E



-- import Model exposing (..)
-- import Views exposing (..)


type alias Model =
    { loginState : ViewLoginState
    , userLoadState : LoadState
    , userPatchState : LoadState
    , errormsg : Maybe String
    , editState : EditState
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { loginState =
            { username = "fefe"
            , password = "123456"
            , state = LoadingStateIdle
            }
      , userLoadState = LoadingStateIdle
      , userPatchState = LoadingStateIdle
      , editState =
            { goalEditState = Nothing
            , attributeEditState = Nothing
            }
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


type ViewPatchState
    = LoadState


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
    { name : String
    , description : Maybe String
    , deadline : Maybe String
    }


type alias EditState =
    { goalEditState : Maybe GoalEditState
    , attributeEditState : Maybe AttributeEditState
    }


type alias GoalEditState =
    { id : Int
    , content : Attribute
    }


type alias AttributeEditState =
    { id : Int
    , content : Attribute
    }


type LoadState
    = LoadingStateIdle
    | LoadingStateWait
    | LoadingStateError (Maybe Http.Error)
    | LoadingStateSuccess (Maybe SuccessContent)
    | LoginStateSuccess String


type SuccessContent
    = UserContent_ UserContent


type LoadedPart
    = LoadedAttribute (Result Http.Error Attribute)


type ViewMessage
    = UpdateUsername String
    | UpdatePassword String
    | UpdateAttribute AttributeKey


type AttributeKey
    = Short Int String
    | Name Int String
    | Description Int String
    | Send Int


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

        ViewMessages (UpdateAttribute (Short index value)) ->
            case model.userLoadState of
                LoadingStateSuccess successcontent ->
                    case successcontent of
                        Just (UserContent_ content) ->
                            let
                                eCont =
                                    case Array.get index content.attributes of
                                        Just att_ ->
                                            { content | attributes = Array.set index { att_ | short = value } content.attributes }

                                        Nothing ->
                                            content
                            in
                            ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent_ eCont), userPatchState = LoadingStateWait }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( { model | userLoadState = LoadingStateError <| Just Http.NetworkError }, Cmd.none )

        ViewMessages (UpdateAttribute (Name index value)) ->
            case model.userLoadState of
                LoadingStateSuccess successcontent ->
                    case successcontent of
                        Just (UserContent_ content) ->
                            let
                                eCont =
                                    case Array.get index content.attributes of
                                        Just att_ ->
                                            { content | attributes = Array.set index { att_ | name = value } content.attributes }

                                        Nothing ->
                                            content
                            in
                            ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent_ eCont), userPatchState = LoadingStateWait }, Cmd.none )

                        Nothing ->
                            ( { model | userLoadState = LoadingStateError Nothing }, Cmd.none )

                _ ->
                    ( { model | userLoadState = LoadingStateError <| Just Http.NetworkError }, Cmd.none )

        ViewMessages (UpdateAttribute (Description index value)) ->
            case model.userLoadState of
                LoadingStateSuccess successcontent ->
                    case successcontent of
                        Just (UserContent_ content) ->
                            let
                                eCont =
                                    case Array.get index content.attributes of
                                        Just att_ ->
                                            { content | attributes = Array.set index { att_ | description = Just value } content.attributes }

                                        Nothing ->
                                            content
                            in
                            ( { model | userLoadState = LoadingStateSuccess (Just <| UserContent_ eCont), userPatchState = LoadingStateWait }, Cmd.none )

                        Nothing ->
                            ( { model | userLoadState = LoadingStateError Nothing }, Cmd.none )

                _ ->
                    ( { model | userLoadState = LoadingStateError <| Just Http.NetworkError }, Cmd.none )

        ViewMessages (UpdateAttribute (Send index)) ->
            let
                h =
                    Debug.log "received" "send attribute update"
            in
            case model.userLoadState of
                LoadingStateSuccess successcontent ->
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
                                            ( model, updateAttributes token value )

                                        _ ->
                                            ( model, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( { model | userLoadState = LoadingStateError Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadedPart (LoadedAttribute result) ->
            case result of
                Ok att ->
                    ( { model | userPatchState = LoadingStateSuccess Nothing }, Cmd.none )

                Err error ->
                    ( { model | errormsg = Just "Error while updating" }, Cmd.none )


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
            Debug.todo "?? what is happening. Loading without success"


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
    D.map3 Goal
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
                                    [ div []
                                        [ text "Loaded Successfully" ]
                                    , div []
                                        [ text ("email: " ++ user.email) ]
                                    , div []
                                        [ text "Attributes: "
                                        , user.attributes
                                            |> Array.indexedMap
                                                (\index att ->
                                                    case att.description of
                                                        Just desc ->
                                                            [ Html.form [ Html.Events.onSubmit <| ViewMessages <| UpdateAttribute <| Send index ]
                                                                [ input
                                                                    [ value att.short
                                                                    , onInput (ViewMessages << UpdateAttribute << Short index)
                                                                    ]
                                                                    []
                                                                , input
                                                                    [ value att.name
                                                                    , onInput (ViewMessages << UpdateAttribute << Name index)
                                                                    ]
                                                                    []
                                                                , input
                                                                    [ value desc
                                                                    , onInput (ViewMessages << UpdateAttribute << Description index)
                                                                    ]
                                                                    []
                                                                , div [] []
                                                                ]
                                                            ]

                                                        -- [ att.short, ": ", att.name, " (", desc, ")" ]
                                                        Nothing ->
                                                            [ Html.form [ Html.Events.onSubmit <| ViewMessages <| UpdateAttribute <| Send index ]
                                                                [ input
                                                                    [ value att.short
                                                                    , onInput (ViewMessages << UpdateAttribute << Short index)
                                                                    ]
                                                                    []
                                                                , input
                                                                    [ value att.name
                                                                    , onInput (ViewMessages << UpdateAttribute << Name index)
                                                                    ]
                                                                    []
                                                                ]
                                                            ]
                                                )
                                            |> Array.map
                                                (\item ->
                                                    item
                                                        -- |> List.map div []
                                                        |> li []
                                                        |> List.singleton
                                                        |> div []
                                                )
                                            |> Array.toList
                                            |> ul []
                                            |> List.singleton
                                            |> div []

                                        -- |> viewTextListToUl
                                        ]
                                    , div [] []
                                    , div []
                                        [ text "Goals:" ]

                                    -- , user.goals
                                    --     |> Array.map
                                    --         (\goal ->
                                    --             case goal.description of
                                    --                 Just desc ->
                                    --                     [ goal.name, " (", desc, ")" ]
                                    --                 Nothing ->
                                    --                     [ goal.name ]
                                    --         )
                                    --     -- |> List.map
                                    --     |> Array.indexedMap text
                                    ]

                                Nothing ->
                                    [ text "An error occured. No Usercontent was loaded" ]

                        LoadingStateError error ->
                            [ div []
                                (viewResponseError error)
                            , div []
                                [ text ("Logged in! Token: " ++ token)
                                , div []
                                    [ button [ onClick LoadUserContent ] [ text "Load Content" ]
                                    ]
                                ]
                            ]

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
                    viewResponseError error

                _ ->
                    [ text "oh no" ]
            )
        ]



-- viewLoginForm : ViewLoginState -> Html Msg -> List (Html Msg)


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


viewResponseError : Maybe Http.Error -> List (Html Msg)
viewResponseError err =
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



-- viewTextToLi : List String -> Html Msg
-- viewTextToLi textList =
--     textList
--         |> String.concat
--         |> text
--         |> List.singleton
--         |> (++) [ button [ onClick (ViewMessages ViewEditLItem) ] [ text "Edit" ] ]
--         |> List.reverse
--         |> li []
-- viewTextListToUl : List (List String) -> Html Msg
-- viewTextListToUl liList =
--     liList
--         |> List.map viewTextToLi
--         |> Html.ul []
--         |> List.singleton
--         |> div []
