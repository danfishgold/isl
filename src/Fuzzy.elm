module Fuzzy exposing (debugGrade, filter, filterItems)

import String
import Regex


filter : Int -> String -> List String -> List ( String, ( List Int, Int ) )
filter minLetters query strings =
    filterItems minLetters query identity strings


filterItems : Int -> String -> (a -> String) -> List a -> List ( a, ( List Int, Int ) )
filterItems minLetters query itemString items =
    if List.length (filteredQuery query) < minLetters then
        []
    else
        List.filterMap (match query itemString) items
            |> List.sortBy (Tuple.second >> Tuple.second)


filteredQuery : String -> List String
filteredQuery query =
    query
        |> Regex.find Regex.All (Regex.regex "[א-ת0-9a-zA-Z]")
        |> List.map .match
        |> List.map String.toLower


match : String -> (a -> String) -> a -> Maybe ( a, ( List Int, Int ) )
match query itemString item =
    filteredQuery query
        |> List.indexedMap (,)
        |> List.foldl folder (Just ( String.toLower <| itemString item, [] ))
        |> Maybe.andThen
            (\( _, dists ) ->
                case grade dists of
                    Nothing ->
                        Nothing

                    Just grd ->
                        Just ( item, ( dists, grd ) )
            )


folder : ( Int, String ) -> Maybe ( String, List Int ) -> Maybe ( String, List Int )
folder ( letterIdx, char ) restOfWord =
    case restOfWord of
        Nothing ->
            Nothing

        Just ( word, dists ) ->
            case String.indexes char word of
                [] ->
                    Nothing

                index :: _ ->
                    Just ( String.dropLeft (index + 1) word, dists ++ [ index ] )


grade : List Int -> Maybe Int
grade dists =
    let
        jumps =
            dists |> List.drop 1 |> List.filter (\n -> n > 1) |> List.length
    in
        if jumps > 1 then
            Nothing
        else
            dists |> List.indexedMap distanceGrade |> List.sum |> Just


distanceGrade : Int -> Int -> Int
distanceGrade letterIdx dist =
    if dist == 0 then
        0
    else
        dist + 2


debugGrade : String -> String -> Maybe ( String, ( List Int, Int ) )
debugGrade query word =
    match query identity word
