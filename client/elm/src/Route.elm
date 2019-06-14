module Route exposing (Route(..), fromUrl, href, parser, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | Login
    | App
    | Error
    | Register



-- | Register


parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map App (s "app")
        , Parser.map Error (s "error")
        , Parser.map Register (s "register")
        ]



-- Public helpers


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


replaceUrl key route =
    Nav.replaceUrl key <| routeToString route


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- Internal


routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                -- Register ->
                --     [ "register" ]
                App ->
                    [ "app" ]

                Error ->
                    [ "error" ]

                Register ->
                    [ "register" ]
    in
    "#/" ++ String.join "/" pieces
