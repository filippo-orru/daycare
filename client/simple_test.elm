module Main exposing (update)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseOut, onMouseOver)
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


type alias AppModel =
    { session : Session
    , viewState : Maybe ViewState
    , attributes : Array Attribute
    }


type alias Attribute =
    { short : String
    , name : String
    , description : Maybe String
    }


type ViewState
    = Editing ContentKey
    | Hovering ContentKey


type ContentKey
    = AttributeKey Int Attribute


type EditState
    = EditAttName Int
    | EditAttShort Int
    | NoEdit


viewApp : AppModel -> List (Html AppMsg)
viewApp model =
    [ div []
        [ text "app"
        , a [ Route.href Route.Login ] [ text "login" ]
        ]
    , Array.indexedMap
        (\index att ->
            div []
                [ let
                    vS =
                        model.viewState

                    key =
                        AttributeKey index att

                    description =
                        case att.description of
                            Just desc ->
                                desc

                            Nothing ->
                                "no description"

                    btn t =
                        button [ onClick <| ClickExpand key ] [ text t ]

                    att_ =
                        [ text att.short, text " - ", text att.name ]
                  in
                  div
                    [ onMouseOver <| HoverEnter key
                    , onMouseOut <| HoverLeave key
                    ]
                    (if vS == Just (Editing (AttributeKey index att)) then
                        att_ ++ [ btn "⇧", div [] [ text description ] ]

                     else if vS == Just (Hovering (AttributeKey index att)) then
                        att_ ++ [ btn "⇩" ]

                     else
                        att_
                    )
                ]
        )
        model.attributes
        |> Array.toList
        |> div []

    -- , text <| Debug.toString model.editing
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


type AppMsg
    = EditAttributeShort Int
    | EditAttributeName Int
    | UpdateAttributeShort Int String
    | UpdateAttributeName Int String
    | HoverEnter ContentKey
    | HoverLeave ContentKey
    | ClickExpand ContentKey



-- | Retract ContentKey


updateApp : AppMsg -> AppModel -> ( AppModel, Cmd msg )
updateApp msg model =
    let
        none mmodel =
            ( mmodel, Cmd.none )

        editing =
            case model.viewState of
                Just (Editing _) ->
                    True

                _ ->
                    False
    in
    case msg of
        HoverEnter key ->
            if editing then
                none model

            else
                none { model | viewState = Just <| Hovering key }

        HoverLeave key ->
            if editing then
                none model

            else
                none { model | viewState = Nothing }

        ClickExpand key ->
            if model.viewState == Just (Editing key) then
                none { model | viewState = Nothing }

            else
                none { model | viewState = Just <| Editing key }

        _ ->
            none model



{- let
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
-}
-- updateEditState transform model =
--     { model | editing = transform model.editing }
-- ( _, _ ) ->
--     ( model, Cmd.none )


updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


appinit : Session -> ( AppModel, Cmd msg )
appinit session =
    ( { session = session

      --   , editing = { state = NoEdit, content = Nothing }
      , attributes =
            Array.fromList
                [ { short = "o", name = "one", description = Nothing }
                , { short = "t", name = "two", description = Just "Second att" }
                , { short = "U", name = "threee", description = Just "ehh" }
                ]
      , viewState = Nothing
      }
    , Cmd.none
    )


logininit : Session -> ( LoginModel, Cmd msg )
logininit session =
    ( { username = "", session = session }, Cmd.none )


homeinit : Session -> ( HomeModel, Cmd msg )
homeinit session =
    ( { session = session }, Cmd.none )
