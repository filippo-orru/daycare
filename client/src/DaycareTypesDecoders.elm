module DaycareTypesDecoders exposing
    ( Attribute
    , Date
    , Day
    , Goal
    , Model
    , Msg(..)
    , Name
    , Task
    , UTime
    , User
    , User2
    , UserID
    , UserLoadResponse
    , UserLoadingState(..)
    , UserLoginResponse
    , UserLoginState(..)
    , attributeDecoder
    , goalDecoder
    , setLoadingState
    , setLoginState
    , setUsername
    , userDecoder
    , userLoadEncoder
    , userLoadResponseDecoder
    , userLoginEncoder
    , userLoginResponseDecoder
    )

import Dict exposing (Dict)
import Http exposing (Error)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { loginState : UserLoginState
    , loadingState : UserLoadingState
    , username : String
    , password : String
    }


type Msg
    = UserLoadResponded (Result Http.Error UserLoadResponse)
    | UserLoginResponded (Result Http.Error UserLoginResponse)
      -- = LoadUser String String
    | Login
    | SetUsername String
    | SetPassword String
    | UserLoginStateReset


type UserLoginState
    = LoginStateLoggedIn String -- token
    | LoginStateLoggedOut
    | LoginStateFail (Maybe Http.Error)


type UserLoadingState
    = LoadStateIdle
    | LoadStateLoading
    | LoadStateSuccess User2 -- user
    | LoadStateFail (Maybe Http.Error)



-- type alias FormState =
--     { username : String }


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


type alias UserID =
    { username : String
    , password : String
    }


type alias User2 =
    { sid : Int
    , username : String
    , password : String
    , attributes : List Attribute
    , goals : List Goal
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


type alias UserLoadResponse =
    { success : Bool
    , user : Maybe User2
    }


type alias UserLoginResponse =
    { success : Bool
    , token : Maybe String
    }



-- type Success
--     = Successful
--     | Unsuccessful
-- setters


setLoadingState : Model -> UserLoadingState -> Model
setLoadingState model state =
    { model | loadingState = state }


setLoginState : Model -> UserLoginState -> Model
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
    D.map5 User2
        (D.field "sid" D.int)
        (D.field "username" D.string)
        (D.field "password" D.string)
        (D.field "attributes" (D.list attributeDecoder))
        (D.field "goals" (D.list goalDecoder))



-- (D.field "days" daysDecoder)


attributeDecoder : D.Decoder Attribute
attributeDecoder =
    D.map3 Attribute
        (D.field "name" D.string)
        (D.field "short" D.string)
        (D.maybe (D.field "description" D.string))


goalDecoder : D.Decoder Goal
goalDecoder =
    D.map3 Goal
        (D.field "name" D.string)
        (D.maybe (D.field "description" D.string))
        (D.maybe (D.field "deadline" D.string))



-- daysDecoder =
--     let
--         l=  (D.field
-- dayDecoder : D.Decoder Day
-- dayDecoder =
--     D.map3 Day


userLoginResponseDecoder : D.Decoder UserLoginResponse
userLoginResponseDecoder =
    D.map2 UserLoginResponse
        (D.field "success" D.bool)
        (D.maybe (D.field "token" D.string))


userLoadResponseDecoder : D.Decoder UserLoadResponse
userLoadResponseDecoder =
    D.map2 UserLoadResponse
        (D.field "success" D.bool)
        (D.maybe (D.field "content" userDecoder))


userLoginEncoder : String -> String -> E.Value
userLoginEncoder u p =
    E.object
        [ ( "username", E.string u )
        , ( "password", E.string p )
        ]


userLoadEncoder : String -> E.Value
userLoadEncoder token =
    E.object
        -- [ ( "username", E.string username )
        -- , ( "password", E.string password )
        [ ( "token", E.string token ) ]



-- "token")
-- successLoginDecoder success =
--     D.field "token" D.string
-- successDecoder : String -> a -> b -> Bool -> D.Decoder b
-- successDecoder field successRes failRes success =
--     case success of
--         True ->
--             ( D.succeed successRes
--             , D.field "token" D.string
--             )
--         False ->
--             D.succeed failRes
-- D.map2 UserLoginSuccess
--     (D.field "")
-- successfulDecoder : Bool -> D.Decoder Success
-- successfulDecoder success =
--     case success of
--         True ->
--             D.succeed Successful
--         False ->
--             D.succeed Unsuccessful