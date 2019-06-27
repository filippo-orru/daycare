module Dialog exposing (Dialog, DialogType(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Dialog msg =
    { type_ : DialogType
    , title : String
    , body : String
    , buttonText : String

    -- , buttonAction : DialogMsg msg
    , buttonAction : msg
    }


type DialogType
    = Error
    | Info



-- type DialogMsg msg
--     = DialogMsg msg


view : Dialog msg -> Html msg
view dialog =
    let
        icon =
            i
                [ class
                    (case dialog.type_ of
                        Error ->
                            "dialog fas fa-times"

                        Info ->
                            "dialog fas fa-info"
                    )
                ]
                []
    in
    div [ class "dialog darken-overlay" ]
        [ div [ class "dialog container" ]
            [ div [ class "dialog-header" ]
                [ icon, h3 [ class "dialog-title" ] [ text dialog.title ] ]
            , div [ class "dialog-body" ]
                [ span [ class "dialog-body-text" ] [ text dialog.body ]
                , button [ class "dialog-body-button", onClick dialog.buttonAction ] [ text dialog.buttonText ]
                ]
            ]
        ]
