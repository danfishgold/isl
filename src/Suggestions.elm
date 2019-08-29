module Suggestions exposing (suggestions)

import Array
import Colors
import Dictionary exposing (WordId)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick, onMouseEnter)
import Element.Font as Font
import Fuzzy
import Query exposing (Query)


suggestions : (WordId -> msg) -> (Int -> msg) -> Maybe Int -> Query WordId -> List (Element.Attribute msg) -> Element msg
suggestions selectWord setSelectedIndex selectedIndex query attrs =
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
                                Background.color Colors.suggestions.selectedFill

                              else
                                Background.color Colors.suggestions.unselectedFill
                            , if selectedIndex == Just idx then
                                Font.color Colors.suggestions.selectedText

                              else
                                Font.color Colors.suggestions.unselectedText
                            ]
                            (Fuzzy.textElement match)
                    )
                |> column attrs
