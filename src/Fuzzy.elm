module Fuzzy exposing (Match, filter, textElement)

import Element exposing (Element, el, paragraph)
import Element.Font as Font


type Match
    = Match ( String, Int, Int )


filter : String -> (a -> String) -> List a -> List ( a, Match )
filter query itemString items =
    List.filterMap (match query itemString) items
        |> List.sortBy (\( _, match_ ) -> startIndex match_)


match : String -> (a -> String) -> a -> Maybe ( a, Match )
match query itemString item =
    let
        str =
            itemString item
    in
    str
        |> String.toLower
        |> String.indexes (String.toLower query)
        |> List.head
        |> Maybe.map (\idx -> ( item, Match ( str, idx, String.length query ) ))


startIndex : Match -> Int
startIndex (Match ( _, start, _ )) =
    start


textElement : Match -> Element msg
textElement match_ =
    let
        ( before, during, after ) =
            splitString match_
    in
    paragraph [ Element.width Element.shrink, Element.height Element.shrink ]
        [ Element.text before
        , el [ Font.bold ] (Element.text during)
        , Element.text after
        ]


splitString : Match -> ( String, String, String )
splitString (Match ( string, idx, length )) =
    ( String.left idx string
    , String.slice idx (idx + length) string
    , String.dropLeft (idx + length) string
    )
