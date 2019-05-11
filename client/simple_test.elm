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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Model
    = Login LoginModel
    | Main MainModel


type alias LoginModel =
    { username : String
    , password : String
    }


type alias MainModel =
    { patchState : PatchState }


type alias PatchState =
    { state : PatchLoadState
    , editingKey : Maybe Editstate
    }


type PatchLoadState
    = PatchIdle
    | PatchWait
    | PatchSuccess
    | PatchError


type Editstate
    = Attribute_ AttributeKey
    | Goal_ GoalKey


type AttributeKey
    = AShort Int String
    | AName Int String


type GoalKey
    = GName Int String
    | GDesc Int String


type Msg
    = GotLoginMessage LoginMsg


type LoginMsg
    = UpdateUsername String
    | UpdatePassword String


type alias NestedRecord =
    { content : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Login { username = "fefe", password = "123456" }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotLoginMessage subMsg, Login login ) ->
            loginUpdate subMsg login

        ( _, _ ) ->
            ( model, Cmd.none )


loginUpdate subm =
    ()


view : Model -> Html Msg
view model =
    div [] []
