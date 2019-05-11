module Model exposing (..)
import Array exposing (Array)
import Http

type alias Model =
    { loginState : ViewLoginState
    , userLoadState : LoadState
    , viewPatchState : ViewPatchState
    , dayLoadState : LoadState

    -- , dayPatchState : ViewPatchState
    , errormsg : Maybe String
    }

type alias ViewLoginState =
    { username : String
    , password : String
    , state : LoadState
    }


type alias ViewPatchState =
    { state : LoadState
    , editing : Maybe EditState
    }


type alias User =
    { email : String
    , attributes : Array Attribute
    , goals : Array Goal
    }


type alias Attribute =
    { short_ : String
    , short : String
    , name : String
    , description : Maybe String
    }


type alias Goal =
    { name_ : String
    , name : String
    , description : Maybe String
    , deadline : Maybe String
    }


type alias Days =
    Array Day


type alias Day =
    { date_ : Date
    , date : Date
    , description : Maybe String
    , attributes : Maybe (List String)
    , tasks : List Task
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


type EditState
    = GEditState Int Goal
    | AEditState Int Attribute
    | DEditState Day
    | DTaskEditState Date Task


type alias Date =
    String



-- type KeyTypes
--     = KeyGoal
--     | KeyAttribute


type LoadState
    = LoadingStateIdle
    | LoadingStateWait
    | LoadingStateError (Maybe Http.Error)
    | LoadingStateSuccess (Maybe SuccessContent)
    | LoginStateSuccess String


type SuccessContent
    = UserContent User
    | DaysContent Days


type UserPart
    = LoadedAttribute (Result Http.Error Attribute)
    | LoadedGoal (Result Http.Error Goal)


type ViewMessage
    = UpdateUsername String
    | UpdatePassword String
    | UpdateAttribute AttributeKey
    | UpdateGoal GoalKey
    | UpdateDay DayKey


type AttributeKey
    = AShort Int String
    | AName Int String
    | ADescription Int String
    | ASend Int


type GoalKey
    = GName Int String
    | GDescription Int String
    | GSend Int


type DayKey
    = DDate Int String
    | DDescription Int String