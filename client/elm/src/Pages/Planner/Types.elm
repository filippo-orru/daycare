module Pages.Planner.Types exposing (Attribute, AttributeKey(..), ContentKey(..), ContentKeyW(..), DTPart(..), Day, DayPartKey(..), DayTaskKey(..), DaysLoadState(..), EditState(..), Goal, GoalKey(..), GuestModel, GuestMsg(..), LoadedModel, LoadedMsg(..), LoggedinModel, LoggedinMsg(..), PartVisibility, RangeDay, SettingsPart(..), StateModel(..), StateMsg(..), Task, TaskState(..), Time, User, UserLevel(..), UserLoadState(..), UserPart(..), UserPatchState, ViewLoadState(..), ViewState)

import Array exposing (Array)
import Browser.Dom
import Date exposing (Date)
import Http
import Session exposing (Session)


type StateModel
    = Guest GuestModel
    | Loggedin LoggedinModel
    | Loaded LoadedModel


type alias GuestModel =
    { session : Session
    , today : Date
    }


type alias LoggedinModel =
    { session : Session
    , token : String
    , user : Maybe (Result Http.Error User)
    , days : Maybe (Result Http.Error (Array Day))
    , dayRange : ( Date, Date )
    , today : Date
    }


type alias LoadedModel =
    { session : Session
    , token : String
    , user : User
    , days : Array Day
    , viewState : ViewState
    , today : Date
    , partVis : PartVisibility

    -- , dialog : Maybe (Dialog (Html StateMsg))
    }


type alias ViewState =
    { loading : Maybe ViewLoadState

    -- , hovering : Maybe ContentKey
    , editing : Maybe ContentKey
    , adding : Maybe ContentKey
    , dayRange : ( Date, Date )
    , dayInFocus : ( Int, Float )
    }


type alias PartVisibility =
    { sidebar : Bool
    , settingsOverlay : Maybe SettingsPart
    }


type SettingsPart
    = SettingsOverview
    | SettingsAccount
    | SettingsSettings
    | SettingsHide


type StateMsg
    = GuestMsg GuestMsg
    | LoggedinMsg LoggedinMsg
    | LoadedMsg LoadedMsg


type GuestMsg
    = GuestNoOp
      -- | TodayGuest Date
    | LoadedToken String


type LoggedinMsg
    = LoadedDays (Result Http.Error (Array Day))
    | LoadedUser (Result Http.Error User)
    | Today Date
    | LogoutL



-- | LoadedToken String


type LoadedMsg
    = UpdatedUser (Result Http.Error User)
    | UpdatedDays (Result Http.Error (Array Day))
    | LoadMoreDays
    | UpdateAttributePart AttributeKey String
    | UpdateGoalPart ContentKey GoalKey
    | Synchronize (Result Http.Error ())
    | EditStart ContentKey
    | EditUpdate ContentKeyW
    | EditDiscard
    | EditCommit
    | EditRemove ContentKey
    | ToggleSidebar
    | AddStart ContentKeyW
    | AddUpdate String
    | AddDiscard
    | AddCommit
    | ToggleDayTask DTPart Int Task Int
      -- | EditDayTask Int Task Int
    | AddDay Int Date
    | SynchronizeLoad (Cmd StateMsg)
    | KeyDown Int Bool
    | NoOp
    | Logout
    | ShowSettings SettingsPart
    | ScrolledDays
    | ComputedDayDistance Int (Result Browser.Dom.Error Browser.Dom.Element)


type UserLoadState
    = UserLIdle
    | UserLWait
    | UserLError Http.Error
    | UserLSuccess User


type DaysLoadState
    = DaysLIdle
    | DaysLWait
    | DaysLError Http.Error
    | DaysLSuccess (Array Day)


type alias UserPatchState =
    { state : UserLoadState
    , editing : Maybe EditState
    }


type ViewLoadState
    = ViewLLoading
    | ViewLError Http.Error
    | ViewLSuccess


type alias User =
    { email : String
    , attributes : Array Attribute
    , goals : Array Goal
    , level : UserLevel
    }


type alias Attribute =
    { name_ : String
    , name : String
    , short : String
    , description : Maybe String
    }


type alias Goal =
    { name_ : String
    , name : String
    , description : Maybe String
    , deadline : Maybe String
    }


type UserLevel
    = LevelUser
    | LevelMod
    | LevelAdmin


type EditState
    = GEditState Int Goal
    | AEditState Int Attribute
    | DEditState Day
    | DTaskEditState Date Task


type UserPart
    = LoadedAttribute (Result Http.Error Attribute)
    | LoadedGoal (Result Http.Error Goal)



-- type PartKey


type AttributeKey
    = AShort String
    | AName String
    | ADescription String



-- | ASend Int


type GoalKey
    = GName String
    | GDescription String
    | GSend String


type DayPartKey
    = DDate String
    | DDescription String
    | DTask Int Task


type DTPart
    = DTState
    | DTImportant



-- | DTaskPart Int DayTaskKey
-- type DayPartKeyW
--     = DDateW String
--     | DDescriptionW String
--     | DTaskW Int


type DayTaskKey
    = DTName String
    | DTState_


type ContentKey
    = AttributeKey Int Attribute
    | GoalKey Int Goal
      -- | DayTaskKey Int Task Int
    | DayPartKey Int DayPartKey


type ContentKeyW
    = AttributeKeyW (Maybe AttributeKey)
    | GoalKeyW (Maybe GoalKey)
    | DayTaskKeyW Int Int (Maybe DayTaskKey)
    | DayPartKeyW Int (Maybe DayPartKey)


type alias RangeDay =
    { date : Date
    , day : Maybe Day
    }


type alias Day =
    { date_ : Date
    , date : Date

    -- , owner : String
    , description : Maybe String
    , attributes : List String
    , tasks : Array Task
    , isPosted : Bool
    }


type alias Task =
    { name_ : String
    , name : String
    , state : TaskState
    , time : Maybe Time
    , important : Bool
    }


type TaskState
    = TSTodo
    | TSCompleted
    | TSP75
    | TSP50
    | TSP25


type alias Time =
    { pre : Int
    , start : Float
    , end : Float
    , post : Int
    }
