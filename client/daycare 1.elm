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
    ( { loginState = LoggedOut
      , loadingState = Idle
      , username = ""
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
        LoadUser username password ->
            ( { model | loadingState = Loading }, loadUser username password )

        LoadedUser result ->
            case result of
                Ok user ->
                    ( { model
                        | loadingState = Success
                        , loginState = LoggedIn
                        , user = Just user
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | loadingState = Failure error }, Cmd.none )

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        Login ->
            ( model, login model )


view : Model -> Html Msg
view model =
    case model.loginState of
        LoggedIn ->
            loggedInViewGenerate model

        LoggedOut ->
            case model.loadingState of
                Failure _ ->
                    text "error logging you in"

                Loading ->
                    text "Logging you in..."

                Success ->
                    text "there is some fucking mismatch in the model states"

                Idle ->
                    div []
                        [ text "not logged in"
                        , div []
                            [ input [ type_ "text", value model.username, onInput SetUsername, placeholder "username" ] []
                            , button [ onClick Login ] [ text "login" ]
                            ]
                        ]


loadUser : String -> String -> Cmd Msg
loadUser username password =
    Http.post
        { url = "http://127.0.0.1:5000/api/v2/get/user"
        , body = Http.jsonBody (userIdent username password)
        , expect =
            Http.expectJson LoadedUser userResponseDecoder
        }



-- login : Model -> model


login model =
    let
        username =
            model.username
    in
    loadUser username ""


loggedInViewGenerate : Model -> Html msg
loggedInViewGenerate model =
    div [ class "main" ]
        [ text "logged in"
        , div [ class "sidebar" ]
            (sidebarViewGenerate model)

        -- []
        , div [ class "planner" ]
            -- []
            [ plannerViewGenerate
                model
            ]
        ]


sidebarViewGenerate : Model -> List (Html msg)
sidebarViewGenerate model =
    -- let
    --     user =
    --         case model.user of
    --             Just user_ ->
    --                 user_
    --             _ ->
    --                 Nothing
    -- in
    -- let
    --     localusername =
    --         user.username
    --     localpassword =
    --         user.password
    -- in
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
            [ text "no user data" ]


plannerViewGenerate : Model -> Html msg
plannerViewGenerate model =
    case model.loadingState of
        Loading ->
            text "UserLoading..."

        Failure error ->
            text (Debug.toString error)

        --"Failed to load data. Reload"
        Success ->
            case model.user of
                Just user ->
                    text ("Welcome " ++ user.username)

                Nothing ->
                    text ""

        _ ->
            text ""



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
