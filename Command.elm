module Command exposing (..)

import Model exposing (..)
import Regex exposing (..)
import Debug
import Char


buildRegex : String -> (String -> List Match)
buildRegex regexStr =
    find (AtMost 1) (regex ("(" ++ regexStr ++ ")") |> caseInsensitive)


kingRegex : String -> List Match
kingRegex =
    buildRegex "king"


queenRegex : String -> List Match
queenRegex =
    buildRegex "queen"


knightRegex : String -> List Match
knightRegex =
    buildRegex "knight"


bishopRegex : String -> List Match
bishopRegex =
    buildRegex "bishop"


pawnRegex : String -> List Match
pawnRegex =
    buildRegex "pawn"


rookRegex : String -> List Match
rookRegex =
    buildRegex "rook|castle"


findRank : String -> Rank
findRank str =
    if kingRegex str |> List.length |> (==) 1 then
        (King False)
    else if queenRegex str |> List.length |> (==) 1 then
        Queen
    else if bishopRegex str |> List.length |> (==) 1 then
        Bishop
    else if knightRegex str |> List.length |> (==) 1 then
        Knight
    else if pawnRegex str |> List.length |> (==) 1 then
        Pawn
    else if rookRegex str |> List.length |> (==) 1 then
        (Rook False)
    else
        None



-- Returns (-1, -1) if no position is given


findPosition : String -> ( Int, Int )
findPosition str =
    let
        matches =
            find (AtMost 1) (regex "([a|b|c|d|e|f|g|h])\\s*(\\d)" |> caseInsensitive) str

        _ =
            Debug.log (toString matches) 1
    in
        case List.head matches of
            Just match ->
                case match.submatches of
                    [ Just col, Just row ] ->
                        -- Geting character is anoying
                        let
                            char =
                                col |> String.toList |> List.head

                            -- Integer value of col
                            col_ =
                                case char of
                                    Just c ->
                                        (Char.toCode c) - (Char.toCode 'a')

                                    _ ->
                                        -1
                        in
                            ( Result.withDefault -1 (String.toInt row), col_ )

                    _ ->
                        ( -1, -1 )

            _ ->
                ( -1, -1 )
