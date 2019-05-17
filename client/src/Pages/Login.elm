module Pages.Login exposing (LoadState(..), Model, Msg(..), decodeUserLogin, encodeUserLogin, init, login, update, view, viewLoginForm)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Route
import Session exposing (Session)



-- Model


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , username = "fefe"
      , password = "123456"
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
    | LoginStateError (Maybe Http.Error)
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

                _ ->
                    text ""
    in
    viewLoginForm model append


viewLoginForm : Model -> Html Msg -> List (Html Msg)
viewLoginForm model appendix =
    [ div []
        [ text "Username:"
        , input [ placeholder "username", value model.username, onInput UpdateUsername ] []
        ]
    , div []
        [ text "Password:"
        , input [ placeholder "Password", value model.password, onInput UpdatePassword ] []
        ]
    , div []
        [ button [ onClick UserLoginLoad ] [ text "Login" ] ]
    , div [] [ appendix ]
    ]



-- Update


type Msg
    = UserLoginLoad
    | UserLoginLoaded (Result Http.Error String)
    | UpdateUsername String
    | UpdatePassword String
    | LoadPlanner


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
            ( { model | state = LoginStateError <| Just message }, Cmd.none )

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
