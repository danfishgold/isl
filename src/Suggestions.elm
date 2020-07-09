module Suggestions exposing (suggestions)

import Array
import Colors
import Dictionary exposing (WordId)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick, onMouseEnter)
import Element.Font as Font
import Fuzzy
import Localization as L10n exposing (Locale)
import Query exposing (Query)


suggestions : Locale -> (WordId -> msg) -> (Int -> msg) -> Maybe Int -> Query WordId -> List (Element.Attribute msg) -> Element msg
suggestions locale selectWord setSelectedIndex selectedIndex query attrs =
    case Maybe.map Array.toList (Query.suggestions query) of
        Nothing ->
            Element.none

        Just [] ->
            row Nothing Nothing False (text <| L10n.string locale (.search >> .noResults)) attrs

        Just matches ->
            matches
                |> List.indexedMap
                    (\idx ( word, match ) ->
                        row (Just <| selectWord word)
                            (Just <| setSelectedIndex idx)
                            (selectedIndex == Just idx)
                            (Fuzzy.textElement match)
                            []
                    )
                |> column attrs


row : Maybe msg -> Maybe msg -> Bool -> Element msg -> List (Element.Attribute msg) -> Element msg
row maybeOnClick maybeOnHover isSelected content attrs =
    let
        props =
            List.concat
                [ [ width fill
                  , height shrink
                  , padding 5
                  ]
                , case maybeOnClick of
                    Just onClick_ ->
                        [ onClick onClick_ ]

                    Nothing ->
                        []
                , case maybeOnHover of
                    Just onHover ->
                        [ onMouseEnter onHover ]

                    Nothing ->
                        []
                , if isSelected then
                    [ Background.color Colors.suggestions.selectedFill
                    , Font.color Colors.suggestions.selectedText
                    ]

                  else
                    [ Background.color Colors.suggestions.unselectedFill
                    , Font.color Colors.suggestions.unselectedText
                    ]
                , attrs
                ]
    in
    el props content
