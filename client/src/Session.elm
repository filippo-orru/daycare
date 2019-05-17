module Session exposing (Session(..), cred, fromString, navKey)

import Browser.Navigation as Nav


type Session
    = LoggedIn Nav.Key String
    | Guest Nav.Key


cred : Session -> Maybe String
cred session =
    case session of
        LoggedIn key token ->
            Just token

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
