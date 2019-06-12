module Days exposing (Day, Days, LoadState(..), LoadStatee(..), Model, Task, TaskState(..), TimeD, decodeDate, decodeDay, decodeDays, decodeState, decodeTask, decodeTime, encodeDay, encodeTask, encodeTasks, updatePart, viewDay, viewDays)

import Array exposing (Array)
import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Http
import Json.Decode as D
import Json.Encode as E



-- updateDayPart : Model -> DayKey -> ( Model, Cmd Msg )


type alias Model =
    { patchState : Maybe LoadState
    , loadState : Maybe LoadState
    , editing : Int
    }


type LoadState
    = Success
    | Failure (Maybe Http.Error)


updatePart model dayPart =
    let
        vps =
            model.viewPatchState

        errReturn =
            { model
                | viewPatchState =
                    { editing = Nothing
                    , state = LoadingStateError Nothing
                    }
            }

        -- ( key, index, value ) =
        --     case dayPart of
        --         DDate i v ->
        --             ( "date", i, v )
        --         DDescription i v ->
        --             ( i, v )
        eh =
            ""

        -- maybeday =
        --     Array.get index days
        -- -- TODO: either get this to work this way around
        -- -- or just copy structure of updateGoalPart :/
        -- Array.set index
        -- content = case maybeday of
        --     Nothing ->
        --         { day |  }
    in
    case model.dayLoadState of
        LoadingStateSuccess days ->
            -- Just day ->
            --     ( { model | userLoadState = LoadingStateSuccess <| Just day }, Cmd.none )
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


decodeDays : D.Decoder Days
decodeDays =
    D.array decodeDay


decodeDay : D.Decoder Day
decodeDay =
    D.map5 Day
        (D.field "date" decodeDate)
        (D.field "date" decodeDate)
        (D.maybe <| D.field "description" D.string)
        (D.maybe <| D.field "attributes" <| D.list D.string)
        (D.field "tasks" <| D.array decodeTask)


decodeDate : D.Decoder Date
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
                        String.slice 6 8 datestr
                            |> D.decodeString D.int
                in
                case ( year, month, day ) of
                    ( Ok yr, Ok mo, Ok da ) ->
                        D.succeed (Date.fromCalendarDate yr mo da)

                    _ ->
                        D.fail "could not create date"
            )


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

                    "75%" ->
                        D.succeed TSP75

                    "50%" ->
                        D.succeed TSP50

                    "25%" ->
                        D.succeed TSP25

                    "todo" ->
                        D.succeed TSTodo

                    _ ->
                        D.fail <| "Unknown state: " ++ str
            )


decodeTime : D.Decoder TimeD
decodeTime =
    D.map4 TimeD
        (D.field "pre" D.int)
        (D.field "start" D.string)
        (D.field "end" D.string)
        (D.field "post" D.int)


type alias Days =
    Array Day


type alias Day =
    { date_ : Date
    , date : Date
    , description : Maybe String
    , attributes : Maybe (List String)
    , tasks : Array Task
    }


type alias Task =
    { name_ : String
    , name : String
    , state : TaskState
    , time : Maybe TimeD
    }


type TaskState
    = TSTodo
    | TSCompleted
    | TSP75
    | TSP50
    | TSP25


type alias TimeD =
    { pre : Int
    , start : String
    , end : String
    , post : Int
    }


type LoadStatee
    = LoadingStateIdle
    | LoadingStateWait
    | LoadingStateError (Maybe Http.Error)
    | LoadingStateSuccess Days


viewDays days patchState =
    div []
        [ text "Days:"
        , days
            |> Array.map
                (\day -> lazy2 viewDay day patchState)
            -- (\_ -> text <| Debug.toString)
            |> Array.toList
            |> ul []
            |> List.singleton
            |> div []
        ]



-- viewDay : Day -> PatchState -> Html msg


viewDay day patchState =
    let
        patching =
            patchState.state == LoadingStateWait
    in
    li []
        [ div []
            [ Html.form
                []
                [ input
                    [ value day.date
                    , disabled patching
                    , alt "Date"
                    ]
                    []
                , case day.description of
                    Just desc ->
                        input
                            [ value desc
                            , disabled patching
                            , alt "Description"
                            ]
                            []

                    Nothing ->
                        text ""
                , case day.attributes of
                    Just attributes ->
                        attributes
                            |> List.map
                                (\att ->
                                    att
                                        |> text
                                        |> List.singleton
                                        |> li []
                                 -- |> List.singleton
                                )
                            -- |> div []
                            -- |> List.singleton
                            |> ul []

                    Nothing ->
                        text ""

                -- , case patchState.editing of
                --     Just (DEditState day_) ->
                --         if day_.date_ == day.date then
                --             if patching then
                --                 text "(...)"
                --             else
                --                 input [ value "Ok", type_ "submit" ] []
                --         else
                --             text ""
                --     _ ->
                --         text ""
                ]
            ]
        ]


encodeDay : Day -> E.Value
encodeDay day =
    E.object
        [ ( "description", E.string (Maybe.withDefault "" day.description) )
        , ( "tasks", encodeTasks day.tasks )
        ]


encodeTasks : Array Task -> E.Value
encodeTasks tasks =
    E.array encodeTask tasks


encodeTask : Task -> E.Value
encodeTask task =
    E.object
        [ ( "name", E.string task.name )
        , ( "status"
          , E.string <|
                case task.state of
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
        ]
