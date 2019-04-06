module CardParser exposing (Suit(..), Value(..))

import Html exposing (text)


type Value
    = Jack
    | Queen
    | King
    | Ace
    | Num Int


type Suit
    = Club
    | Diamond
    | Spade
    | Heart


type alias Card =
    ( Value, Suit )


printSuit suit =
    Debug.toString suit


printValue : Value -> String
printValue value =
    case value of
        Num 2 ->
            "Two"

        Num 3 ->
            "Three"

        Num 4 ->
            "Four"

        Num 5 ->
            "Five"

        Num 6 ->
            "Six"

        Num 7 ->
            "Seven"

        Num 8 ->
            "Eight"

        Num 9 ->
            "Nine"

        Num 10 ->
            "Ten"

        _ ->
            Debug.toString value


printCard : Card -> String
printCard ( value, suit ) =
    [ printValue value, " of ", printSuit suit ] |> String.concat


parseSuit : Char -> Maybe Suit
parseSuit char =
    case char of
        'C' ->
            Just Club

        'D' ->
            Just Diamond

        'S' ->
            Just Spade

        'H' ->
            Just Heart

        _ ->
            Nothing


parseValue : String -> Maybe Value
parseValue v =
    case v of
        "J" ->
            Just Jack

        "Q" ->
            Just Queen

        "K" ->
            Just King

        "A" ->
            Just Ace

        _ ->
            parseNumValue v


parseNumValue : String -> Maybe Value
parseNumValue v =
    case String.toInt v of
        Just num ->
            if num >= 2 && num <= 10 then
                Just (Num num)

            else
                Nothing

        _ ->
            Nothing


divideCardString : String -> ( Maybe String, Maybe Char )
divideCardString str =
    let
        chars =
            String.toList str

        suit =
            chars
                |> List.reverse
                |> List.head

        value =
            chars
                |> List.reverse
                |> List.tail
                |> Maybe.map List.reverse
                |> Maybe.map String.fromList
    in
    ( value, suit )


main =
    "AH"
        |> divideCardString
        |> Debug.toString
        |> text
