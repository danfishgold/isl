module Fuzzy exposing (filter)

import Element exposing (Element, el, paragraph)
import Element.Font as Font


filter : String -> (a -> String) -> List a -> List ( a, Element msg )
filter query itemString items =
    List.filterMap (simpleMatch query itemString) items
        |> List.sortBy Tuple.second
        |> List.map
            (\( item, matchIndex ) ->
                ( item
                , simpleElement
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


simpleElement : String -> Int -> String -> Element msg
simpleElement string startingIndex query =
    let
        ( before, during, after ) =
            splitString (String.length query) startingIndex string
    in
    paragraph [ Element.width Element.shrink, Element.height Element.shrink ]
        [ Element.text before
        , el [ Font.bold ] (Element.text during)
        , Element.text after
        ]


splitString : Int -> Int -> String -> ( String, String, String )
splitString length idx string =
    ( String.left idx string
    , String.slice idx (idx + length) string
    , String.dropLeft (idx + length) string
    )
