port module Pages.Login exposing (LoadState(..), Model, Msg(..), decodeUserLogin, encodeUserLogin, init, login, subscriptions, update, view, viewLoginForm)

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
      , username = ""
      , password = ""
      , state = LoginStateIdle
      }
    , Cmd.none
    )


type alias Model =
    { session : Session
    , username : String
    , password : String
    , state : LoadState
    }


type LoadState
    = LoginStateIdle
    | LoginStateWait
    | LoginStateError Http.Error
    | LoginStateSuccess String



-- View


view : Model -> List (Html Msg)
view model =
    let
        append =
            case model.state of
                LoginStateWait ->
                    text "..."

                LoginStateSuccess _ ->
                    text "Success!"

                LoginStateError Http.NetworkError ->
                    text "Cannot reach Server. Please retry."

                LoginStateError _ ->
                    text "An error occurred. Please retry."

                LoginStateIdle ->
                    text ""
    in
    [ div [ class "login container" ] [ viewLoginForm model append ] ]


viewLoginForm : Model -> Html Msg -> Html Msg
viewLoginForm model appendix =
    Html.form [ class "login form", onSubmit UserLoginLoad ]
        [ h3 [ class "login header" ] [ text "LOGIN" ]

        -- , label [ class "login username" ] [ text "Username:" ]
        , input [ class "login username", placeholder "username", value model.username, onInput UpdateUsername, autofocus True, autocomplete True ] []

        -- , label [ class "login password" ] [ text "Password:" ]
        , input [ class "login password", placeholder "password", value model.password, onInput UpdatePassword ] []
        , button [ class "login submit", type_ "submit" ] [ text "Login" ]
        , div [ class "login appendix" ] [ appendix ]
        ]



-- Update


type Msg
    = UserLoginLoad
    | UserLoginLoaded (Result Http.Error String)
    | UpdateUsername String
    | UpdatePassword String
    | LoadPlanner
    | LoadedToken String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UserLoginLoad, _ ) ->
            ( { model | state = LoginStateWait }, login model )

        ( UserLoginLoaded (Ok token), _ ) ->
            let
                key =
                    Session.navKey model.session
            in
            ( { model | session = Session.fromString key (Just token) }, Route.replaceUrl key Route.App )

        ( UserLoginLoaded (Err message), _ ) ->
            ( { model | state = LoginStateError message }, Cmd.none )

        ( UpdateUsername u, _ ) ->
            ( { model | username = u }, Cmd.none )

        ( UpdatePassword p, _ ) ->
            ( { model | password = p }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



--HTTP


login : Model -> Cmd Msg
login model =
    Http.post
        { url = "http://localhost:5000/api/v3/login"
        , body = Http.jsonBody (encodeUserLogin model)
        , expect = Http.expectJson UserLoginLoaded decodeUserLogin
        }



-- Json


encodeUserLogin : Model -> E.Value
encodeUserLogin model =
    E.object
        [ ( "username", E.string model.username )
        , ( "password", E.string model.password )
        ]


decodeUserLogin : D.Decoder String
decodeUserLogin =
    D.field "token" D.string



{- PORTS -}


subscriptions : Model -> Sub Msg
subscriptions model =
    loadedToken LoadedToken


port loadedToken : (String -> msg) -> Sub msg
