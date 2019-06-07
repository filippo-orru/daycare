module Pages.Home exposing (Model, Msg(..), init, view)

import Html exposing (..)
import Html.Attributes exposing (..)
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
    [ div [ class "home header" ]
        [ div [ class "home header-header-container" ]
            [ h1 [ class "home header-header" ] [ text "daycare" ] ]
        ]
    , div [ class "home body" ]
        [ div [ class "home hero hero-main" ]
            [ h3 [ class "home hero-main-text" ] [ text "hero" ] ]
        ]

    -- , a [ Route.href Route.App ] [ text "Planner" ]
    ]
