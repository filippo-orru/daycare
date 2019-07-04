module Pages.Register exposing (LoadState(..), Model, Msg(..), init, update, view, viewForm)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import Pages.Login as Login
import Random
import Route
import Session exposing (Session)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , email = ""
      , username = ""
      , password = ""
      , state = LIdle
      , randStr = "25072001"
      }
    , Random.generate GotRandStr generateRandStr
    )


type alias Model =
    { session : Session
    , email : String
    , username : String
    , password : String
    , randStr : String
    , state : LoadState
    }


type LoadState
    = LIdle
    | LWait
    | LError Http.Error
    | LSuccess


type Msg
    = Register
    | Registered (Result Http.Error String)
    | UpdateEmail String
    | UpdateUsername String
    | UpdatePassword String
    | GotRandStr (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmail e ->
            ( { model | email = e }, Cmd.none )

        UpdatePassword p ->
            ( { model | password = p }, Cmd.none )

        Register ->
            ( model, register model.email model.password model.randStr )

        Registered (Ok token) ->
            ( { model | state = LSuccess }, Cmd.none )

        Registered (Err err) ->
            ( { model | state = LError err }, Cmd.none )

        GotRandStr list ->
            ( { model | randStr = String.concat <| List.map String.fromInt list }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> List (Html Msg)
view model =
    let
        errorspan txt =
            span [ class "error-text" ] [ text txt ]

        append =
            case model.state of
                LWait ->
                    span [ class "wait-text" ] [ text "..." ]

                LSuccess ->
                    span [ class "success-text" ] [ text "Success!" ]

                LError Http.NetworkError ->
                    errorspan "Cannot reach Server."

                LError (Http.BadStatus 401) ->
                    errorspan "Wrong identifier or password!"

                LError (Http.BadStatus 409) ->
                    errorspan "A user with this email already exists!"

                LError _ ->
                    errorspan "An error occurred. Please retry."

                _ ->
                    text ""
    in
    Login.viewContainer (viewForm model append) False


viewForm : Model -> Html Msg -> Html Msg
viewForm model append =
    Html.form [ class "form", onSubmit Register ]
        [ input
            [ class "identifier"
            , placeholder "email"
            , value model.email
            , type_ "email"
            , onInput UpdateEmail
            , autofocus True
            , autocomplete True
            , attribute "aria-label" "email input"
            , tabindex 0
            ]
            []
        , input
            [ class "password"
            , placeholder "password"
            , value model.password
            , type_ "password"
            , onInput UpdatePassword
            , attribute "aria-label" "password input"
            , tabindex 0
            ]
            []
        , button [ class "submit", type_ "submit" ] [ text "Register" ]
        , div [ class "appendix" ] [ append ]
        ]


register : String -> String -> String -> Cmd Msg
register email password randStr =
    Http.post
        { url = "/api/v3/users"
        , body = Http.jsonBody (encodeUser email password randStr)
        , expect = Http.expectJson Registered decodeRegistered
        }


encodeUser : String -> String -> String -> E.Value
encodeUser email password randStr =
    let
        username =
            case List.head (String.indexes "@" email) of
                Just index ->
                    String.left index email ++ "_" ++ randStr

                _ ->
                    "sweet-potato_" ++ randStr
    in
    E.object
        [ ( "email", E.string email )
        , ( "username", E.string username )
        , ( "password", E.string password )
        ]


decodeRegistered =
    D.field "token" D.string


generateRandStr : Random.Generator (List Int)
generateRandStr =
    Random.list 8 (Random.int 0 9)
