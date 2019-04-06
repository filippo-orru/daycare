module Main exposing (update)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { clist : Array NestedRecord }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init () =
    ( { clist =
            Array.fromList
                [ { content = "Initial content" }
                , { content = "Ini" }
                , { content = "hahaha" }
                ]
      }
    , Cmd.none
    )


type Msg
    = Update Int String


type alias NestedRecord =
    { content : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update i input ->
            -- let
            --     i =
            --         ""
            --     -- String.slice ";" input
            -- in
            case Array.get i model.clist of
                Just value ->
                    ( { model | clist = Array.set i { value | content = input } model.clist }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            (model.clist
                |> Array.toList
                |> List.map (\item -> item.content ++ "  ")
                |> List.map text
            )
        , div []
            (model.clist
                |> Array.indexedMap
                    (\index item ->
                        input
                            [ value item.content
                            , Html.Events.onInput (Update index)
                            ]
                            []
                    )
                |> Array.toList
            )
        ]



-- todo: take intelligence from here and apply back to daycare.elm
