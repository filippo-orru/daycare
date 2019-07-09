module Pages.Planner.Rest exposing (decodeAttribute, decodeAttributeKey, decodeDate, decodeDay, decodeDays, decodeFullTime, decodeGoal, decodeGoalKey, decodeLevel, decodeRangeDay, decodeRangeDays, decodeState, decodeTask, decodeTime, decodeUser, encodeAttribute, encodeDay, encodeGoal, encodeState, encodeTask, encodeTasks, loadDays, loadUser, patchAttribute, patchDay, patchGoals, postAttribute, postDay)

-- import Html.Attributes

import Array exposing (Array)
import Date exposing (Date)
import Html
import Html.Events exposing (keyCode, on)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Encode as E
import Pages.Planner.Types exposing (..)


loadUser : (Result Http.Error User -> StateMsg) -> String -> Cmd StateMsg
loadUser msg token =
    -- let
    --     msg =
    --         case model of
    --             Loggedin _ ->
    --                 LoggedinMsg << LoadedUser
    --             Loaded _ ->
    --                 LoadedMsg << LoadedUser
    -- in
    Http.request
        { method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me"
        , expect = Http.expectJson msg decodeUser
        }


loadDays : (Result Http.Error (Array Day) -> StateMsg) -> String -> ( Date, Date ) -> Cmd StateMsg
loadDays msg token range =
    let
        limit =
            Date.diff Date.Days (Tuple.second range) (Tuple.first range)
                * 5
                |> String.fromInt

        -- |> Debug.log "limit"
    in
    Http.request
        { method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.emptyBody
        , headers = [ Http.header "token" token, Http.header "limit" limit ]
        , url = "/api/v3/users/me/days"
        , expect = Http.expectJson msg (decodeDays range)
        }


postAttribute : String -> Attribute -> Cmd StateMsg
postAttribute token attribute =
    Http.request
        { method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeAttribute attribute
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me/attributes"
        , expect = Http.expectWhatever (LoadedMsg << Synchronize)
        }


patchAttribute : String -> Attribute -> Cmd StateMsg
patchAttribute token attribute =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeAttribute attribute
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me/attributes/" ++ attribute.name_
        , expect = Http.expectWhatever (LoadedMsg << Synchronize)
        }


patchGoals : String -> Goal -> Cmd StateMsg
patchGoals token goal =
    Http.request
        { method = "PATCH"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeGoal goal
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me/goals/" ++ goal.name_
        , expect = Http.expectWhatever (LoadedMsg << Synchronize)
        }


postDay token day =
    let
        body =
            Debug.log "newdaypost" day
    in
    Http.request
        { method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeDay day
        , headers = [ Http.header "token" token ]
        , url = "/api/v3/users/me/days"
        , expect = Http.expectWhatever (LoadedMsg << Synchronize)
        }


patchDay : String -> Day -> Cmd StateMsg
patchDay token day =
    let
        ( method, url ) =
            if day.isPosted then
                ( "PATCH", "/api/v3/users/me/days/" ++ Date.format "YYYYMMdd" day.date_ )

            else
                ( "POST", "/api/v3/users/me/days" )
    in
    Http.request
        { method = method
        , timeout = Nothing
        , tracker = Nothing
        , body = Http.jsonBody <| encodeDay day
        , headers = [ Http.header "token" token ]
        , url = url
        , expect = Http.expectWhatever (LoadedMsg << Synchronize)
        }


decodeDays : ( Date, Date ) -> Decoder (Array Day)
decodeDays range_ =
    let
        range =
            Date.range Date.Day 1 (Date.add Date.Days -1 <| Tuple.second range_) (Date.add Date.Days 1 <| Tuple.first range_)
                |> List.reverse

        -- |> Debug.log "range of days"
    in
    D.array decodeDay
        -- |> D.andThen (Debug.log "array of days")
        |> D.andThen (decodeRangeDays range)


decodeRangeDays : List Date -> Array Day -> Decoder (Array Day)
decodeRangeDays range days =
    List.map (decodeRangeDay days) range
        |> Array.fromList
        |> D.succeed


decodeRangeDay : Array Day -> Date -> Day
decodeRangeDay days date =
    let
        filtered =
            Array.filter (\day -> day.date == date) days
    in
    case List.head <| Array.toList <| filtered of
        -- maybeday
        Just day ->
            day

        Nothing ->
            newDay date


newDay : Date -> Day
newDay date =
    Day date date Nothing [] (Array.fromList []) False



-- else
--     RangeDay date Nothing


decodeDay : Decoder Day
decodeDay =
    D.map6 Day
        (D.field "date" decodeDate)
        (D.field "date" decodeDate)
        -- (D.field "owner" D.string)
        (D.maybe <| D.field "description" D.string)
        (D.withDefault [] <| D.field "attributes" <| D.list D.string)
        (D.field "tasks" <| D.array decodeTask)
        (D.succeed True)


decodeDate : Decoder Date
decodeDate =
    D.string
        |> D.andThen
            (\datestr ->
                let
                    year =
                        String.slice 0 4 datestr
                            |> D.decodeString D.int

                    month =
                        (if String.startsWith "0" <| String.slice 4 6 datestr then
                            String.slice 5 6 datestr

                         else
                            String.slice 4 6 datestr
                        )
                            |> D.decodeString D.int
                            |> (\mr ->
                                    case mr of
                                        Ok m ->
                                            Ok (Date.numberToMonth m)

                                        Err _ ->
                                            Err "failed to decode month"
                               )

                    day =
                        (if String.startsWith "0" <| String.slice 6 8 datestr then
                            String.slice 7 8 datestr

                         else
                            String.slice 6 8 datestr
                        )
                            |> D.decodeString D.int
                in
                case ( year, month, day ) of
                    ( Ok yr, Ok mo, Ok da ) ->
                        D.succeed (Date.fromCalendarDate yr mo da)

                    _ ->
                        let
                            _ =
                                ( year, month, day )
                                    |> Debug.log "failed date parts"
                        in
                        D.fail "could not create date"
            )


decodeTask : Decoder Task
decodeTask =
    D.map5 Task
        (D.field "name" D.string)
        (D.field "name" D.string)
        (D.field "state" decodeState |> D.withDefault TSTodo)
        (D.optionalField "time" decodeTime)
        (D.field "important" decodeImportant |> D.withDefault False)


decodeImportant : Decoder Bool
decodeImportant =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "true" ->
                        D.succeed True

                    "false" ->
                        D.succeed False

                    _ ->
                        D.fail "malformed important value"
            )


decodeTime : Decoder Time
decodeTime =
    D.map4 Time
        (D.field "pre" decodeIntTime)
        (D.field "start" decodeFullTime)
        (D.field "end" decodeFullTime)
        (D.field "post" decodeIntTime)


decodeIntTime : D.Decoder Int
decodeIntTime =
    D.string
        |> D.andThen
            (\ms ->
                case String.toInt ms of
                    Just s ->
                        D.succeed s

                    Nothing ->
                        D.fail "Invalid number in time of task"
                            |> Debug.log "failed decode inttime"
            )


decodeFullTime : Decoder Float
decodeFullTime =
    D.string
        |> D.andThen
            (\timestr ->
                let
                    startstr =
                        String.slice 0 2 timestr

                    endstr =
                        String.slice 2 4 timestr
                in
                case ( String.toFloat startstr, String.toFloat endstr ) of
                    ( Just startfloat, Just endfloat ) ->
                        (D.succeed <| startfloat + endfloat / 60)
                            |> Debug.log "succeeded fulltime decoding"

                    _ ->
                        D.fail "time has invalid format"
                            |> Debug.log "failed decode fulltime"
            )



-- (D.field "state" decodeState)
-- (D.maybe <| D.field "time" decodeTime)


decodeState : Decoder TaskState
decodeState =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "completed" ->
                        D.succeed TSCompleted

                    "75%" ->
                        D.succeed TSP75

                    "50%" ->
                        D.succeed TSP50

                    "25%" ->
                        D.succeed TSP25

                    _ ->
                        D.succeed TSTodo
            )



-- decodeTime : Decoder TimeD
-- decodeTime =
--     D.map4 TimeD
--         (D.field "pre" D.int)
--         (D.field "start" D.string)
--         (D.field "end" D.string)
--         (D.field "post" D.int)


encodeDay : Day -> E.Value
encodeDay day =
    E.object
        [ ( "date", E.string <| Date.format "YYYYMMdd" day.date )

        -- , ( "owner", E.string day.owner )
        , ( "description", E.string <| Maybe.withDefault "" day.description )
        , ( "attributes", (E.list <| E.string) day.attributes )
        , ( "tasks", encodeTasks day.tasks )
        ]


encodeTasks : Array Task -> E.Value
encodeTasks tasks =
    E.array encodeTask tasks


encodeTask : Task -> E.Value
encodeTask task =
    E.object
        [ ( "name", E.string task.name )
        , ( "state", encodeState task.state )
        , ( "important", encodeBool task.important )
        ]


encodeBool bool =
    if bool then
        E.string "true"

    else
        E.string "false"


encodeState state =
    E.string
        (case state of
            TSCompleted ->
                "completed"

            TSTodo ->
                "todo"

            TSP75 ->
                "75%"

            TSP50 ->
                "50%"

            TSP25 ->
                "25%"
        )



-- main


decodeUser : D.Decoder User
decodeUser =
    D.map4 User
        (D.field "email" D.string)
        (D.field "attributes" (D.array decodeAttribute))
        (D.field "goals" (D.array decodeGoal))
        (D.field "level" decodeLevel)


decodeAttribute : D.Decoder Attribute
decodeAttribute =
    D.map4 Attribute
        (D.field "short" D.string)
        (D.field "short" D.string)
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))


decodeAttributeKey : D.Decoder (Maybe ContentKey)
decodeAttributeKey =
    D.map Just <| D.map (AttributeKey 0) decodeAttribute


decodeGoal : D.Decoder Goal
decodeGoal =
    D.map4 Goal
        (D.field "name" D.string)
        (D.field "name" D.string)
        (D.maybe <| D.field "description" D.string)
        (D.maybe <| D.field "deadline" D.string)


decodeGoalKey : D.Decoder ContentKey
decodeGoalKey =
    D.map (GoalKey 0) decodeGoal


decodeLevel : D.Decoder UserLevel
decodeLevel =
    D.string
        |> D.andThen
            (\level ->
                case level of
                    "user" ->
                        D.succeed LevelUser

                    "mod" ->
                        D.succeed LevelMod

                    "admin" ->
                        D.succeed LevelAdmin

                    _ ->
                        D.fail "no valid level"
            )


encodeAttribute : Attribute -> E.Value
encodeAttribute attribute =
    let
        desc =
            Maybe.withDefault "" attribute.description
    in
    E.object
        [ ( "short", E.string attribute.short )
        , ( "name", E.string attribute.name )
        , ( "description", E.string desc )
        ]


encodeGoal : Goal -> E.Value
encodeGoal goal =
    E.object
        [ ( "name", E.string goal.name )
        , ( "description", E.string (Maybe.withDefault "" goal.description) )
        ]
