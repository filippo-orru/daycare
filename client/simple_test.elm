module Main exposing (update)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Route
import Session exposing (Session)
import Url exposing (Url)


main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Model
    = Redirect Session
    | Login LoginModel
    | App AppModel
    | NotFound Session
    | Home HomeModel


type alias LoginModel =
    { username : String
    , session : Session
    }


type alias AppModel =
    { session : Session
    , editing : Editing
    , atts : Array Attribute
    }


type alias Attribute =
    { short : String
    , name : String
    , description : Maybe String
    }


type alias Editing =
    { state : EditState
    , content : Maybe String
    }


type EditState
    = EditAttName Int
    | EditAttShort Int
    | NoEdit


type alias HomeModel =
    { session : Session }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url navkey =
    changeRouteTo (Route.fromUrl url) (Redirect (Session.fromString navkey Nothing))


view : Model -> Browser.Document Msg
view model =
    let
        viewPage toMsg body =
            { title = "simpletest"

            -- , body = [ div [] [ text <| Debug.toString model ], div [] body ]
            , body = List.map (Html.map toMsg) [ div [] body ]
            }
    in
    case model of
        Redirect _ ->
            viewPage NoOp [ text "redirect" ]

        NotFound _ ->
            viewPage NoOp [ text "Could not find page.", a [ Route.href Route.App ] [ text "App" ] ]

        App app ->
            viewPage GotAppMsg <| viewApp app

        Login login ->
            viewPage GotLoginMsg
                [ text "login "
                , a [ Route.href Route.App ] [ text "app" ]
                , input [ onInput UpdateUsername ] []
                , div [] []
                , text login.username
                ]

        Home login ->
            viewPage GotHomeMsg
                [ text "home "
                , a [ Route.href Route.Login ] [ text "login " ]
                , a [ Route.href Route.App ] [ text "app " ]
                ]


viewApp : AppModel -> List (Html AppMsg)
viewApp model =
    [ div []
        [ text "app"
        , a [ Route.href Route.Login ] [ text "login" ]
        ]
    , Array.indexedMap
        (\index att ->
            let
                state =
                    model.editing.state
            in
            [ if state == EditAttShort index then
                div [ class "noblock" ]
                    [ input [ value att.short ] [] ]

              else
                div [ class "noblock", onClick (EditAttributeShort index) ]
                    [ text att.short ]
            , if state == EditAttName index then
                div [ class "noblock" ]
                    [ text "Name: ", input [ value att.name, onInput (UpdateAttributeName index) ] [] ]

              else
                div [ class "noblock", onClick (EditAttributeName index) ]
                    [ text ("Name: " ++ att.name) ]
            ]
                |> li []
        )
        model.atts
        |> Array.toList
        |> div []
    , text <| Debug.toString model.editing
    ]


toSession model =
    case model of
        App app ->
            app.session

        Login login ->
            login.session

        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            home.session


type LoginMsg
    = UpdateUsername String


type HomeMsg
    = Homa


type AppMsg
    = EditAttributeShort Int
    | EditAttributeName Int
    | UpdateAttributeShort Int String
    | UpdateAttributeName Int String


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | ChangedRoute Route.Route
    | GotLoginMsg LoginMsg
    | GotAppMsg AppMsg
    | GotHomeMsg HomeMsg
    | NoOp String


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo mayberoute model =
    let
        session =
            toSession model
    in
    case mayberoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.App ->
            appinit session
                |> updateWith App GotAppMsg model

        Just Route.Login ->
            logininit session
                |> updateWith Login GotLoginMsg model

        Just Route.Home ->
            homeinit session
                |> updateWith Home GotHomeMsg model

        Just Route.Error ->
            ( NotFound session, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlrequest, _ ) ->
            case urlrequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Nav.pushUrl (Session.navKey <| toSession model) (Url.toString url)
                            )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo (Just route) model

        -- ( NotFound session, _ ) ->
        --     ( NotFound session, Cmd.none )
        ( GotLoginMsg subMsg, Login login ) ->
            updateLogin subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotAppMsg subMsg, App app ) ->
            updateApp subMsg app
                |> updateWith App GotAppMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateLogin : LoginMsg -> LoginModel -> ( LoginModel, Cmd msg )
updateLogin msg model =
    case ( msg, model ) of
        ( UpdateUsername u, _ ) ->
            ( { model | username = u }, Cmd.none )


updateApp : AppMsg -> AppModel -> ( AppModel, Cmd msg )
updateApp msg model =
    let
        editing =
            model.editing
    in
    case msg of
        EditAttributeShort i ->
            ( { model | editing = { editing | state = EditAttShort i, content = Nothing } }, Cmd.none )

        EditAttributeName i ->
            ( { model | editing = { editing | state = EditAttName i, content = Nothing } }, Cmd.none )

        UpdateAttributeShort i content ->
            ( { model | editing = { editing | content = Just content } }, Cmd.none )

        UpdateAttributeName i content ->
            ( { model | editing = { editing | content = Just content } }, Cmd.none )


updateEditState transform model =
    { model | editing = transform model.editing }



-- ( _, _ ) ->
--     ( model, Cmd.none )


updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


appinit : Session -> ( AppModel, Cmd msg )
appinit session =
    ( { session = session
      , editing = { state = NoEdit, content = Nothing }
      , atts =
            Array.fromList
                [ { short = "o", name = "one", description = Nothing }
                , { short = "t", name = "two", description = Just "Second att" }
                , { short = "U", name = "threee", description = Just "ehh" }
                ]
      }
    , Cmd.none
    )


logininit : Session -> ( LoginModel, Cmd msg )
logininit session =
    ( { username = "", session = session }, Cmd.none )


homeinit : Session -> ( HomeModel, Cmd msg )
homeinit session =
    ( { session = session }, Cmd.none )
