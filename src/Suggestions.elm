module Suggestions exposing (suggestions)

import Dict exposing (Dict)
import Dictionary exposing (Dictionary)
import Element exposing (..)
import Element.Events exposing (onClick)
import Fuzzy


suggestions : Dictionary -> String -> Element msg
suggestions { words, groups } query =
    groups
        |> Dict.toList
        |> Fuzzy.simpleFilterItems query Tuple.first
        |> List.map
            (\( ( _, ids ), paragraph ) ->
                el
                    [ width fill
                    , height shrink
                    ]
                    paragraph
            )
        |> column [ height fill, scrollbarY, spacing 5 ]


sortIds : Dict String String -> List String -> List String
sortIds words ids =
    List.sortBy (\id -> Dict.get id words |> Maybe.withDefault "") ids
