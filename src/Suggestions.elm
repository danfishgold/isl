module Suggestions exposing (suggestions)

import Dict exposing (Dict)
import Dictionary exposing (Dictionary, WordId)
import Element exposing (..)
import Element.Events exposing (onClick)
import Fuzzy


suggestions : (WordId -> msg) -> Dictionary -> String -> Element msg
suggestions selectWord dictionary queryText =
    Dictionary.groupList dictionary
        |> Fuzzy.simpleFilterItems queryText Tuple.first
        |> List.map
            (\( ( _, group ), matchText ) ->
                el
                    [ onClick (selectWord group.primary)
                    , width fill
                    , height shrink
                    ]
                    matchText
            )
        |> column [ height fill, scrollbarY, spacing 5 ]
