module Suggestions exposing (suggestions)

import Array
import Dictionary exposing (Dictionary, WordId)
import Element exposing (..)
import Element.Events exposing (onClick)
import Fuzzy
import Query exposing (Query)


suggestions : (WordId -> msg) -> Query WordId -> Element msg
suggestions selectWord query =
    case Query.suggestions query of
        Nothing ->
            Element.none

        Just matches ->
            Array.toList matches
                |> List.map
                    (\( word, match ) ->
                        el
                            [ onClick (selectWord word)
                            , width fill
                            , height shrink
                            ]
                            (Fuzzy.textElement match)
                    )
                |> column [ height fill, scrollbarY, spacing 5 ]
