module Suggestions exposing (suggestions)

import Array
import Dictionary exposing (Dictionary, WordId)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick, onMouseEnter)
import Fuzzy
import Query exposing (Query)


suggestions : (WordId -> msg) -> (Int -> msg) -> Maybe Int -> Query WordId -> Element msg
suggestions selectWord setSelectedIndex selectedIndex query =
    case Query.suggestions query of
        Nothing ->
            Element.none

        Just matches ->
            Array.toList matches
                |> List.indexedMap
                    (\idx ( word, match ) ->
                        el
                            [ onClick (selectWord word)
                            , onMouseEnter (setSelectedIndex idx)
                            , width fill
                            , height shrink
                            , if selectedIndex == Just idx then
                                Background.color (rgb 0.9 1 1)

                              else
                                Background.color (rgb 1 1 1)
                            ]
                            (Fuzzy.textElement match)
                    )
                |> column [ height fill, scrollbarY, spacing 5 ]
