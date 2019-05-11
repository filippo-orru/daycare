module Json exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E
import Model exposing (..)

encodeAttribute : Attribute -> E.Value
encodeAttribute attribute =
    case attribute.description of
        Just desc ->
            E.object
                [ ( "short", E.string attribute.short )
                , ( "name", E.string attribute.name )
                , ( "description", E.string desc )
                ]

        Nothing ->
            E.object
                [ ( "short", E.string attribute.short )
                , ( "name", E.string attribute.name )
                ]


encodeGoal : Goal -> E.Value
encodeGoal goal =
    case goal.description of
        Just desc ->
            E.object
                [ ( "name", E.string goal.name )
                , ( "description", E.string desc )
                ]

        Nothing ->
            E.object
                [ ( "name", E.string goal.name )
                ]



encodeUserLogin loginState =
    E.object
        [ ( "username", E.string loginState.username )
        , ( "password", E.string loginState.password )
        ]


decodeUserLogin : D.Decoder String
decodeUserLogin =
    D.field "token" D.string


decodeUserLoadState : D.Decoder User
decodeUserLoadState =
    D.map3 User
        (D.field "email" D.string)
        (D.field "attributes" (D.array decodeAttribute))
        (D.field "goals" (D.array decodeGoal))


decodeAttribute : D.Decoder Attribute
decodeAttribute =
    D.map4 Attribute
        (D.field "short" D.string)
        (D.field "short" D.string)
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))

decodeGoal : D.Decoder Goal
decodeGoal =
    D.map4 Goal
        (D.field "name" D.string)
        (D.field "name" D.string)
        (D.maybe <| D.field "description" D.string)
        (D.maybe <| D.field "deadline" D.string)


decodeDays : D.Decoder Days
decodeDays =
    D.array decodeDay


decodeDay : D.Decoder Day
decodeDay =
    D.map5 Day
        (D.field "date" D.string)
        (D.field "date" D.string)
        (D.maybe <| D.field "description" D.string)
        (D.maybe <| D.field "attributes" <| D.list D.string)
        (D.field "tasks" <| D.list decodeTask)


decodeTask : D.Decoder Task
decodeTask =
    D.map4 Task
        (D.field "name" D.string)
        (D.field "name" D.string)
        (D.field "state" decodeState)
        (D.maybe <| D.field "time" decodeTime)


decodeState : D.Decoder TaskState
decodeState =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "completed" ->
                        D.succeed TSCompleted

                    "50%" ->
                        D.succeed TSP50

                    "25%" ->
                        D.succeed TSP25

                    "todo" ->
                        D.succeed TSTodo

                    _ ->
                        D.fail <| "Unknown state " ++ str
            )

decodeTime : D.Decoder TimeD
decodeTime =
    D.map4 TimeD
        (D.field "pre" D.int)
        (D.field "start" D.string)
        (D.field "end" D.string)
        (D.field "post" D.int)