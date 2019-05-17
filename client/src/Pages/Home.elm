module Pages.Home exposing (Model, Msg(..), init, view)

import Html exposing (..)
import Route
import Session exposing (Session)


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session }, Cmd.none )


type alias Model =
    { session : Session }


type Msg
    = NoOp


view : Model -> List (Html Msg)
view model =
    [ text "home"
    , div [] []
    , a [ Route.href Route.App ] [ text "Planner" ]
    ]
