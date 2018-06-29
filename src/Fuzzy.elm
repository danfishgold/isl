module Fuzzy exposing (filter, filterItems)

import String
import Regex


filter : Int -> String -> List String -> List String
filter minLetters query strings =
    filterItems minLetters query identity strings


filterItems : Int -> String -> (a -> String) -> List a -> List a
filterItems minLetters query itemString items =
    if List.length (filteredQuery query) < minLetters then
        []
    else
        List.filterMap (match query itemString) items
            |> List.sortBy Tuple.second
            |> List.map Tuple.first


filteredQuery : String -> List String
filteredQuery query =
    query
        |> Regex.find Regex.All (Regex.regex "[א-ת0-9a-zA-Z]")
        |> List.map .match
        |> List.map String.toLower


match : String -> (a -> String) -> a -> Maybe ( a, Int )
match query itemString item =
    filteredQuery query
        |> List.indexedMap (,)
        |> List.foldl folder (Just ( String.toLower <| itemString item, 0 ))
        |> Maybe.map (\( remainder, grade ) -> ( item, grade ))


folder : ( Int, String ) -> Maybe ( String, Int ) -> Maybe ( String, Int )
folder ( letterIdx, char ) restOfWord =
    case restOfWord of
        Nothing ->
            Nothing

        Just ( word, grade ) ->
            case String.indexes char word of
                [] ->
                    Nothing

                index :: _ ->
                    Just ( String.dropLeft (index + 1) word, grade + distanceGrade index letterIdx )


distanceGrade : Int -> Int -> Int
distanceGrade dist letterIdx =
    if dist == 0 then
        0
    else
        dist + 2
