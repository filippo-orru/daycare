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
        [ a [ class "home header-link", Route.href Route.Register, style "margin-right" "16px" ] [ text "register" ] --, style "left" "30px"
        , a [ class "home header-link", Route.href Route.Login ] [ text "login" ]
        ]
    , div [ class "home container" ]
        [ a [ Route.href Route.App, class "home huge-header" ] [ text "preparing for the service" ]
        ]
    ]


view2 : Model -> List (Html Msg)
view2 model =
    [ div [ class "home header" ]
        [ div [ class "home header-header-container" ]
            [ h1 [ class "home header-header" ] [ text "daycare" ]
            , a [ class "home header-login-link", Route.href Route.App ] [ text "APP" ]
            ]
        ]
    , div [ class "home body" ]
        [ div [ class "home hero hero-main" ]
            [ h3 [ class "home hero-main-text" ] [ text "hero" ] ]
        ]

    -- , a [ Route.href Route.App ] [ text "Planner" ]
    ]
