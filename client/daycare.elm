module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, form, input, li, text, ul)
import Http
import Pages.Home as Home
import Pages.Login as Login
import Pages.Planner as Planner
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


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url navkey =
    changeRouteTo (Route.fromUrl url) (Redirect (Session.fromString navkey Nothing))


type Model
    = Home Home.Model
    | Planner Planner.Model
    | Login Login.Model
    | Redirect Session
    | NotFound Session
    | Error Session


view : Model -> Browser.Document Msg
view model =
    let
        viewPage title toMsg body =
            { title = "daycare - " ++ title
            , body = List.map (Html.map toMsg) [ div [] body ]
            }

        viewSimplePage title ctext =
            { title = title
            , body =
                [ text ctext
                ]
            }
    in
    case model of
        Home home ->
            viewPage "Home" GotHomeMsg (Home.view home)

        Login login ->
            viewPage "Login" GotLoginMsg (Login.view login)

        Planner planner ->
            viewPage "Planner" GotPlannerMsg (Planner.view planner)

        Error error ->
            viewSimplePage "Error" <| "An error occurred.  Model:   " ++ Debug.toString error

        Redirect _ ->
            viewSimplePage "Redirecting" <| "Redirecting..."

        NotFound _ ->
            viewSimplePage "404 - Not Found" "Could not find page. Navigate back home"


type Msg
    = GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotPlannerMsg Planner.Msg
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotPlannerMsg subMsg, Planner planner ) ->
            Planner.update subMsg planner
                |> updateWith Planner GotPlannerMsg model

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

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo mayberoute model =
    let
        session =
            toSession model
    in
    case mayberoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg model

        Just Route.App ->
            Planner.init session
                |> updateWith Planner GotPlannerMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.Error ->
            ( Error session, Cmd.none )


toSession model =
    case model of
        Planner planner ->
            planner.session

        Login login ->
            login.session

        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            home.session

        Error session ->
            session
