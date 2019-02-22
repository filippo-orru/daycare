module DaycareTypesDecoders exposing (Date, Day, LoadingState(..), LoginState(..), Model, Msg(..), Name, Task, UTime, User, User2, UserID, UserResponse, setLoadingState, setLoginState, setUsername, testJson, testResponseDecoder, userDecoder, userIdent, userResponseDecoder)

import Dict exposing (Dict)
import Http exposing (Error)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { loginState : LoginState
    , loadingState : LoadingState
    , username : String
    , user : Maybe User2
    }


type LoginState
    = LoggedIn
    | LoggedOut


type LoadingState
    = Loading
    | Success
    | Failure Http.Error
    | Idle



-- type alias FormState =
--     { username : String }


type Msg
    = LoadUser String String
    | LoadedUser (Result Http.Error User2)
    | Login
    | SetUsername String


type alias UTime =
    { pre : Int
    , start : String
    , end : String
    , post : Int
    }


type alias Name a =
    { name : String
    , content : a
    }


type alias Task =
    { name : String
    , state : String
    , time : Maybe UTime
    }


type alias Day =
    { --  date : String
      attributes : Maybe (List String)
    , description : Maybe String
    , tasks : List Task
    }


type alias Date =
    String


type alias Goal =
    { name : String
    , description : Maybe String
    , deadline : Maybe String
    }


type alias Attribute =
    { name : String
    , short : String
    , description : Maybe String
    }


type alias User2 =
    { sid : Int
    , username : String
    , password : String
    , attributes : List Attribute
    }


type alias User =
    { sid : Int
    , username : String
    , password : String
    , settings : { fullscreen : String }
    , goals :
        List
            { description : Maybe String
            , deadline : Maybe String
            }
    , activities :
        List
            { name : String
            , description : Maybe String
            , category : Maybe String
            , prodlevel : Maybe Int
            , time : Maybe UTime
            }
    , attributes :
        List
            { name : String
            , short : String
            , description : Maybe String
            }
    , days :
        Dict Date Day
    }



-- setters


setLoadingState : Model -> LoadingState -> Model
setLoadingState model state =
    { model | loadingState = state }


setLoginState : Model -> LoginState -> Model
setLoginState model state =
    { model | loginState = state }


setUsername : Model -> String -> Model
setUsername model username =
    -- let
    --     model_ = model
    --     model_username = username
    -- in
    { model | username = username }



-- decoders


userDecoder : D.Decoder User2
userDecoder =
    D.map4 User2
        (D.field "sid" D.int)
        (D.field "username" D.string)
        (D.field "password" D.string)
        (D.field "attributes" (D.list attributeDecoder))


attributeDecoder : D.Decoder Attribute
attributeDecoder =
    D.map3 Attribute
        (D.field "name" D.string)
        (D.field "short" D.string)
        (D.field "description" D.string)


goalDecoder : D.Decoder Goal


goalDeocder =
    D.map2 Goal
        (D.field "description")



-- responseDecoder : D.Decoder Dict.Dict String (String|User2)


type alias UserResponse =
    { success : String
    , content : User2
    }


userResponseDecoder : D.Decoder User2
userResponseDecoder =
    -- D.map2 UserResponse
    -- (D.field "success" D.string)
    D.field "content" userDecoder


type alias UserID =
    { username : String
    , password : String
    }


userIdent : String -> String -> E.Value
userIdent username password =
    E.object
        [ ( "username", E.string username )
        , ( "password", E.string password )
        ]


testJson : String -> E.Value
testJson username =
    E.object [ ( "username", E.string username ) ]


testResponseDecoder : D.Decoder String
testResponseDecoder =
    D.field "username" D.string
