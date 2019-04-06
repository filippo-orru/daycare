module Main exposing (main)

import Browser
import Decoders exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E



-- import Model exposing (..)
-- import Views exposing (..)


type alias Model =
    { loginState : ViewLoginState
    , userLoadState : LoadState
    , errormsg : Maybe Http.Error
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


type alias ViewLoginState =
    { username : String
    , password : String
    , state : LoadState
    }


type UserLoadState
    = LoadState


type alias UserContent =
    { email : String
    , attributes : List Attribute
    , goals : List Goal
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
    | LoadingStateError Http.Error
    | LoadingStateSuccess UserContent
    | LoginStateSuccess String


type ViewMessage
    = ViewSetUsername String
    | ViewSetPassword String
    | ViewEditLItem


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
                    ( setLoginStateLoadingstate model (LoadingStateError message), Cmd.none )

        UserContentLoaded result ->
            case result of
                Ok content ->
                    ( { model | userLoadState = LoadingStateSuccess content }, Cmd.none )

                Err message ->
                    ( { model | userLoadState = LoadingStateError message }, Cmd.none )

        ViewMessages (ViewSetUsername u) ->
            ( setLoginStateUsername model u, Cmd.none )

        ViewMessages (ViewSetPassword p) ->
            ( setLoginStatePassword model p, Cmd.none )

        ViewMessages ViewEditLItem ->
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
        (D.field "attributes" (D.list attributeDecoder))
        (D.field "goals" (D.list goalDecoder))


type alias Attribute =
    { short : String
    , name : String
    , description : Maybe String
    }


attributeDecoder =
    D.map3 Attribute
        (D.field "short" D.string)
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))


type alias Goal =
    { name : String
    , description : Maybe String
    , deadline : Maybe String
    }


goalDecoder =
    D.map3 Goal
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))
        (D.maybe (D.field "deadline" D.string))


view : Model -> Html Msg
view model =
    div []
        (case model.loginState.state of
            LoginStateSuccess token ->
                case model.userLoadState of
                    LoadingStateSuccess user ->
                        [ div []
                            [ text "Loaded Successfully" ]
                        , div []
                            [ text ("email: " ++ user.email) ]
                        , div []
                            [ text "Attributes: "
                            , user.attributes
                                |> List.map
                                    (\att ->
                                        case att.description of
                                            Just desc ->
                                                ( att, [ att.short, ": ", att.name, " (", desc, ")" ] )

                                            Nothing ->
                                                ( att, [ att.short, ": ", att.name ] )
                                    )
                                -- |> Debug.toString
                                -- |> String.join ", "
                                -- |> List.map ()
                                |> List.map

                            -- |> viewTextListToUl
                            ]
                        , div [] []
                        , div []
                            [ text "Goals:" ]
                        , user.goals
                            |> List.map
                                (\goal ->
                                    case goal.description of
                                        Just desc ->
                                            [ goal.name, " (", desc, ")" ]

                                        Nothing ->
                                            [ goal.name ]
                                )
                            -- |> List.map
                            |> viewTextListToUl model
                        ]

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



-- viewLoginForm : ViewLoginState -> Html Msg -> List (Html Msg)


viewLoginForm loginState appendix =
    [ div []
        [ text "Username:"
        , input [ placeholder "username", value loginState.username, onInput (ViewMessages << ViewSetUsername) ] []
        ]
    , div []
        [ text "Password:"
        , input [ placeholder "Password", value loginState.password, onInput (ViewMessages << ViewSetPassword) ] []
        ]
    , div []
        [ button [ onClick LoadUserLogin ] [ text "Login" ] ]
    , div []
        [ appendix ]
    ]


viewResponseError : Http.Error -> List (Html Msg)
viewResponseError error =
    [ case error of
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
    ]


viewTextToLi : List String -> Html Msg
viewTextToLi textList =
    textList
        |> String.concat
        |> text
        |> List.singleton
        |> (++) [ button [ onClick (ViewMessages ViewEditLItem) ] [ text "Edit" ] ]
        |> List.reverse
        |> li []



-- viewTextListToUl : Model -> List (List String) -> Html Msg


viewTextListToUl textList =
    textList
        |> List.map viewTextToLi
        |> Html.ul []
        |> List.singleton
        |> div []
