module Main exposing (init, main, update)

-- import DaycareView exposing (..)

import Browser
import DaycareTypesDecoders exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- init : Model -> ( Model, Cmd Msg )
-- init model =
--     ( setLoadingState model Loading, loadUser "fefe" "123456" )


init : () -> ( Model, Cmd Msg )
init () =
    ( { loginState = LoginStateLoggedOut
      , loadingState = LoadStateIdle
      , username = ""
      , password = ""
      , user = Nothing
      }
    , Cmd.none
      --, loadUser "fefe" "123456"
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserLoginResponded result ->
            case result of
                Ok loginResponse ->
                    if loginResponse.success == True then
                        case loginResponse.token of
                            Just token ->
                                -- case token of
                                --     Just
                                ( { model | loginState = LoginStateLoggedIn token }
                                , loadUser model
                                )

                            -- else
                            --     ( { model | loginState = LoginStateFail Nothing }, Cmd.none )
                            Nothing ->
                                ( { model | loginState = LoginStateFail Nothing }, Cmd.none )

                    else
                        ( { model | loginState = LoginStateFail Nothing }, Cmd.none )

                Err error ->
                    ( { model | loginState = LoginStateFail (Just error) }, Cmd.none )

        UserLoadResponded result ->
            case result of
                Ok loadResponse ->
                    if loadResponse.success == True then
                        case loadResponse.user of
                            Just content ->
                                ( { model | loadingState = LoadStateSuccess, user = Just content }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model | loadingState = LoadStateFail Nothing }, Cmd.none )

                    else
                        ( { model | loadingState = LoadStateFail Nothing }, Cmd.none )

                Err _ ->
                    ( { model | loadingState = LoadStateFail Nothing }, Cmd.none )

        SetUsername u ->
            ( { model | username = u }, Cmd.none )

        SetPassword p ->
            ( { model | password = p }, Cmd.none )

        Login ->
            ( model, login model )

        UserLoginStateReset ->
            ( { model
                | loginState = LoginStateLoggedOut
                , loadingState = LoadStateIdle
              }
            , Cmd.none
            )


loadUser : Model -> Cmd Msg
loadUser model =
    let
        username =
            model.username

        password =
            model.password
    in
    Http.post
        { url = "http://127.0.0.1:5000/api/v2/get/user"
        , body = Http.jsonBody (userIdentEncoder model)
        , expect =
            Http.expectJson UserLoadResponded userResponseDecoder
        }


login : Model -> Cmd Msg
login model =
    let
        username =
            model.username

        password =
            model.password
    in
    Http.post
        { url = "http://127.0.0.1:5000/api/v2/login"
        , body = Http.jsonBody (userIdentEncoder model)
        , expect =
            Http.expectJson UserLoginResponded loginResponseDecoder
        }


view : Model -> Html Msg
view model =
    case model.loginState of
        LoginStateLoggedIn _ ->
            loggedInViewGenerate model

        LoginStateLoggedOut ->
            case model.loadingState of
                LoadStateFail _ ->
                    div []
                        [ text "Weird Error. Shouldn't happen."
                        , button [ onClick UserLoginStateReset ] [ text "retry" ]
                        ]

                LoadStateSuccess ->
                    div []
                        [ text "Weird Error. Shouldn't happen."
                        , button [ onClick UserLoginStateReset ] [ text "retry" ]
                        ]

                LoadStateLoading ->
                    text "Logging you in..."

                LoadStateIdle ->
                    div []
                        [ text "not logged in. "
                        , div []
                            [ input
                                [ type_ "text"
                                , value model.username
                                , onInput SetUsername
                                , placeholder "username"
                                ]
                                []
                            , input
                                [ type_ "text"
                                , value model.password
                                , onInput SetPassword
                                , placeholder "password"
                                ]
                                []
                            , button [ onClick Login ] [ text "login" ]
                            ]
                        ]

        LoginStateFail error ->
            div []
                [ text "Error while logging you in. "
                , let
                    error_ =
                        case error of
                            Just e ->
                                Debug.toString error

                            Nothing ->
                                ""
                  in
                  case String.length <| error_ of
                    0 ->
                        text "Wrong password? "

                    _ ->
                        div []
                            [ text ("Unknown Error!\n\n" ++ Debug.toString error_)
                            , text ("token: " ++ Debug.toString model.loginState)
                            ]
                , button [ onClick UserLoginStateReset ] [ text "retry" ]
                ]


loggedInViewGenerate : Model -> Html msg
loggedInViewGenerate model =
    div [ class "main" ]
        [ div [ class "sidebar" ]
            (sidebarViewGenerate model)

        -- []
        , div [ class "planner" ]
            -- []
            (plannerViewGenerate model)

        -- , div []
        --     [ text ("LogInState:" ++ Debug.toString model.loginState) ]
        ]


sidebarViewGenerate : Model -> List (Html msg)
sidebarViewGenerate model =
    case model.user of
        Just user ->
            [ div [ class "head" ]
                [ h2 [ class "sidebar-header" ]
                    [ text "daycare" ]
                ]
            , div [ class "sidebar-frequent" ]
                [ h3 []
                    [ text "frequent" ]
                , ul [ class "frequent-list" ]
                    (List.map
                        (\a -> li [] [ text a.name ])
                        user.attributes
                    )
                ]
            , div [ class "sidebar-goals" ]
                [ h3 []
                    [ text "goals" ]
                , ul [ class "goals-list" ]
                    [ li [ class "lifegoal" ]
                        [ text "test1" ]
                    , li [ class "lifegoal" ]
                        [ text "test2" ]
                    , li [ class "lifegoal" ]
                        [ text "test23" ]
                    , li [ class "lifegoal" ]
                        [ text "test3" ]
                    ]
                ]
            ]

        Nothing ->
            [ text "Sidebar: no user data to show" ]


plannerViewGenerate : Model -> List (Html msg)
plannerViewGenerate model =
    case model.loadingState of
        LoadStateLoading ->
            [ text "UserLoading..." ]

        LoadStateFail error ->
            [ text (Debug.toString error) ]

        --"Failed to load data. Reload"
        LoadStateSuccess ->
            case model.user of
                Just user ->
                    [ text ("Welcome " ++ user.username) ]

                Nothing ->
                    [ text "Planner: Load Successful but no user data" ]

        _ ->
            [ text "" ]



-- div [ class "days" ]
--     [ ul [ class "days-list" ]
--         [ li [ class "day" ]
--             [ div [ class "day-header" ]
--                 [ h4 []
--                     [ text "Monday, 13th of March 2015" ]
--                 , div [ class "attributes" ]
--                     [ ul [ class "attributes-list" ]
--                         [ li [ class "attribute" ]
--                             [ text "X" ]
--                         , li [ class "attribute" ]
--                             [ text "A" ]
--                         ]
--                     ]
--                 , p [ class "day-description" ]
--                     [ text "Day was very good" ]
--                 ]
--             ]
--         ]
--     ]
