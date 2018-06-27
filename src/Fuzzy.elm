module Fuzzy exposing (..)

import String
import Regex


filter : Int -> String -> List String -> List String
filter minLetters query words =
    if List.length (filteredQuery query) < minLetters then
        []
    else
        List.filterMap (match query) words
            |> List.sortBy Tuple.second
            |> List.map Tuple.first


filteredQuery : String -> List String
filteredQuery query =
    query
        |> Regex.find Regex.All (Regex.regex "[א-ת0-9 ]")
        |> List.map .match


match : String -> String -> Maybe ( String, Int )
match query word =
    filteredQuery query
        |> List.foldl folder (Just ( word, 0 ))
        |> Maybe.map (\( remainder, grade ) -> ( word, grade ))


folder : String -> Maybe ( String, Int ) -> Maybe ( String, Int )
folder char restOfWord =
    case restOfWord of
        Nothing ->
            Nothing

        Just ( word, grade ) ->
            case String.indexes char word of
                [] ->
                    Nothing

                index :: _ ->
                    Just ( String.dropLeft (index + 1) word, grade + index )
