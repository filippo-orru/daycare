module Session exposing (Session(..), fromString, navKey, token)

import Browser.Navigation as Nav


type Session
    = LoggedIn Nav.Key String
    | Guest Nav.Key


token : Session -> Maybe String
token session =
    case session of
        LoggedIn key token_ ->
            Just token_

        Guest key ->
            Nothing


fromString : Nav.Key -> Maybe String -> Session
fromString key maybeString =
    case maybeString of
        Just string ->
            LoggedIn key string

        Nothing ->
            Guest key


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest navkey ->
            navkey

        LoggedIn navkey _ ->
            navkey
