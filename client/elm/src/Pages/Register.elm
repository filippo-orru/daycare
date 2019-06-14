module Pages.Register exposing (LoadState(..), Model, Msg(..), init, update, view, viewForm)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import Route
import Session exposing (Session)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , email = ""
      , username = ""
      , password = ""
      , state = LIdle
      }
    , Cmd.none
    )


type alias Model =
    { session : Session
    , email : String
    , username : String
    , password : String
    , state : LoadState
    }


type LoadState
    = LIdle
    | LWait
    | LError Http.Error
    | LSuccess String


type Msg
    = Register
    | UpdateEmail String
    | UpdateUsername String
    | UpdatePassword String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail e ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> List (Html Msg)
view model =
    let
        append =
            case model.state of
                LWait ->
                    span [ class "login wait-text" ] [ text "..." ]

                LSuccess _ ->
                    span [ class "login success-text" ] [ text "Success!" ]

                LError Http.NetworkError ->
                    span [ class "login error-text" ] [ text "Cannot reach Server" ]

                LError (Http.BadStatus 401) ->
                    span [ class "login error-text" ] [ text "Wrong identifier or password" ]

                LError _ ->
                    span [ class "login error-text" ] [ text "An error occurred. Please retry." ]

                _ ->
                    text ""
    in
    [ div [ class "login container" ]
        [ viewForm model append ]
    ]


viewForm : Model -> Html Msg -> Html Msg
viewForm model append =
    Html.form [ class "login form", onSubmit Register ]
        [ h3 [ class "login header" ] [ text "LOGIN" ]
        , input [ class "login identifier", placeholder "username", value model.username, onInput UpdateUsername, autofocus True, autocomplete True ] []
        , input [ class "login identifier", placeholder "email", value model.email, onInput UpdateEmail, autofocus True, autocomplete True ] []
        , input [ class "login password", placeholder "password", type_ "password", value model.password, onInput UpdatePassword ] []
        , button [ class "login submit", type_ "submit" ] [ text "Login" ]
        , div [ class "login appendix" ] [ append ]
        ]
