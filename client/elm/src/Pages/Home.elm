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
    [ div [ class "header" ]
        [ a [ class "header-link", Route.href Route.Register ] [ text "register" ] --, style "left" "30px"
        , a [ class "header-link", Route.href Route.Login ] [ text "login" ]
        , a [ class "header-link", Route.href Route.App, style "margin-right" "0" ] [ text "app" ]
        ]
    , div [ class "container" ]
        [ a [ Route.href Route.App, class "huge-header" ] [ text "preparing for the service" ]
        ]
    ]


view2 : Model -> List (Html Msg)
view2 model =
    [ div [ class "header" ]
        [ div [ class "header-header-container" ]
            [ h1 [ class "header-header" ] [ text "daycare" ]
            , a [ class "header-login-link", Route.href Route.App ] [ text "APP" ]
            ]
        ]
    , div [ class "body" ]
        [ div [ class "hero hero-main" ]
            [ h3 [ class "hero-main-text" ] [ text "hero" ] ]
        ]

    -- , a [ Route.href Route.App ] [ text "Planner" ]
    ]
