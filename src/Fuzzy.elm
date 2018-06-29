module Fuzzy exposing (debugGrade, filter, filterItems, simpleFilterItems)

import String
import Regex
import Html exposing (Html, span, b, text)


html : String -> List Int -> Html msg
html word dists =
    htmlHelper word dists [] |> span []


htmlHelper : String -> List Int -> List (Html msg) -> List (Html msg)
htmlHelper string dists reversedElements =
    case dists of
        [] ->
            List.reverse <| text string :: reversedElements

        dist :: rest ->
            let
                ( before, char, after ) =
                    splitString 1 dist string
            in
                htmlHelper
                    after
                    rest
                    (b [] [ text char ] :: text before :: reversedElements)


splitString : Int -> Int -> String -> ( String, String, String )
splitString length idx string =
    ( String.left idx string
    , String.slice idx (idx + length) string
    , String.dropLeft (idx + length) string
    )


filter : Int -> String -> List String -> List ( String, Html msg )
filter minLetters query strings =
    filterItems minLetters query identity strings


filterItems : Int -> String -> (a -> String) -> List a -> List ( a, Html msg )
filterItems minLetters query itemString items =
    if List.length (filteredQuery query) < minLetters then
        []
    else
        List.filterMap (match query itemString) items
            |> List.sortBy (Tuple.second >> Tuple.second)
            |> List.map (\( item, ( dists, _ ) ) -> ( item, html (itemString item) dists ))


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


simpleFilterItems : String -> (a -> String) -> List a -> List ( a, Html msg )
simpleFilterItems query itemString items =
    List.filterMap (simpleMatch query itemString) items
        |> List.sortBy Tuple.second
        |> List.map
            (\( item, matchIndex ) ->
                ( item
                , simpleHtml
                    (itemString item)
                    matchIndex
                    query
                )
            )


simpleMatch : String -> (a -> String) -> a -> Maybe ( a, Int )
simpleMatch query itemString item =
    itemString item
        |> String.indexes query
        |> List.head
        |> Maybe.map (\idx -> ( item, idx ))


simpleHtml : String -> Int -> String -> Html msg
simpleHtml string startingIndex query =
    let
        ( before, during, after ) =
            splitString (String.length query) startingIndex string
    in
        span []
            [ text before
            , b [] [ text during ]
            , text after
            ]
