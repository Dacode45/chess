module Util exposing (..)


filterJusts : List (Maybe a) -> List a
filterJusts list =
    case list of
        [] ->
            []

        x :: xs ->
            case x of
                Nothing ->
                    filterJusts xs

                Just just ->
                    just :: filterJusts xs
