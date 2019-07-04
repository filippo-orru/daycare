port module Pages.Login exposing (LoadState(..), Model, Msg(..), decodeUserLogin, encodeUserLogin, init, loginPost, subscriptions, update, view, viewContainer, viewLoginForm)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import Route
import Session exposing (Session)



-- Model


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , identifier = ""
      , password = ""
      , loadstate = LIdle
      }
    , Cmd.none
    )


type alias Model =
    { session : Session
    , identifier : String
    , password : String
    , loadstate : LoadState
    }


type LoadState
    = LIdle
    | LWait
    | LError Http.Error
    | LSuccess String



-- View


view : Model -> List (Html Msg)
view model =
    let
        append =
            case model.loadstate of
                LWait ->
                    span [ class "wait-text" ] [ text "..." ]

                LSuccess _ ->
                    span [ class "success-text" ] [ text "Success!" ]

                LError Http.NetworkError ->
                    span [ class "error-text" ] [ text "Cannot reach Server" ]

                LError (Http.BadStatus 401) ->
                    span [ class "error-text" ] [ text "Wrong identifier or password" ]

                LError _ ->
                    span [ class "error-text" ] [ text "An error occurred. Please retry." ]

                _ ->
                    text ""
    in
    viewContainer (viewLoginForm model append) True


viewContainer : Html msg -> Bool -> List (Html msg)
viewContainer form viewingLogin =
    let
        ( registerClass, loginClass ) =
            if viewingLogin then
                ( "header disabled round-left", "header round-right" )

            else
                ( "header round-left", "header disabled round-right" )
    in
    [ div [ class "fullscreen-container" ]
        [ div [ class "dialog-container" ]
            [ div [ class "dialog-header" ]
                [ a [ class registerClass, Route.href Route.Register, tabindex 0 ]
                    [ span [ class "header-text", Route.href Route.Register ] [ text "REGISTER" ] ]
                , a [ class loginClass, Route.href Route.Login, tabindex 0 ]
                    [ span [ class "header-text", Route.href Route.Login ] [ text "LOGIN" ] ]
                ]
            , div [ class "dialog-body" ]
                [ form ]
            ]
        ]
    ]


viewLoginForm : Model -> Html Msg -> Html Msg
viewLoginForm model appendix =
    Html.form [ class "form", onSubmit UserLoginLoad ]
        [ input
            [ class "identifier"
            , placeholder "username / email"
            , type_ "text"
            , value model.identifier
            , onInput UpdateIdentifier
            , autofocus True
            , autocomplete True
            , tabindex 0
            ]
            []
        , input
            [ class "password"
            , placeholder "password"
            , type_ "password"
            , value model.password
            , onInput UpdatePassword
            , tabindex 0
            ]
            []
        , button [ class "submit", type_ "submit" ] [ text "Login" ]
        , div [ class "appendix" ] [ appendix ]
        ]



-- Update


type Msg
    = UserLoginLoad
    | UserLoginLoaded (Result Http.Error String)
    | UpdateIdentifier String
    | UpdatePassword String
    | UpdateEmail String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserLoginLoad ->
            ( { model | loadstate = LWait }, loginPost model.identifier model.password )

        UserLoginLoaded (Ok token) ->
            let
                key =
                    Session.navKey model.session
            in
            if token /= "" then
                ( { model | session = Session.fromString key (Just token), loadstate = LSuccess token }
                , Cmd.batch [ saveTokenLogin token, Route.replaceUrl key Route.App ]
                )

            else
                ( { model | loadstate = LError <| Http.BadBody "" }, Cmd.none )

        UserLoginLoaded (Err err) ->
            ( { model | loadstate = LError err }, Cmd.none )

        UpdateIdentifier i ->
            ( { model | identifier = i }, Cmd.none )

        UpdatePassword p ->
            ( { model | password = p }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- _ ->
--     ( model, Cmd.none )
--HTTP


loginPost : String -> String -> Cmd Msg
loginPost identifier password =
    Http.post
        { url = "/api/v3/login"
        , body = Http.jsonBody (encodeUserLogin identifier password)
        , expect = Http.expectJson UserLoginLoaded decodeUserLogin
        }



-- Json


encodeUserLogin : String -> String -> E.Value
encodeUserLogin identifier password =
    let
        key =
            if String.contains "@" identifier then
                "email"

            else
                "username"
    in
    E.object
        [ ( key, E.string identifier )
        , ( "password", E.string password )
        ]


decodeUserLogin : D.Decoder String
decodeUserLogin =
    D.field "token" D.string



{- PORTS -}


subscriptions : Model -> Sub Msg
subscriptions model =
    loadedTokenLogin (\t -> UserLoginLoaded <| Ok t)


port loadedTokenLogin : (String -> msg) -> Sub msg


port saveTokenLogin : String -> Cmd msg
