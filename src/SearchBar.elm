module SearchBar exposing (..)

import Html exposing (Html, div, input, p, span, button, text)
import Html.Attributes exposing (value, dir)
import Html.Events exposing (onInput, onClick)
import Dict exposing (Dict)
import Fuzzy
import Dictionary exposing (Dictionary)


view : (String -> msg) -> (List String -> msg) -> (List String -> msg) -> Dictionary -> String -> Html msg
view inputMsg setGroupMsg addGroupMsg { words, groups } query =
    div [ dir "rtl" ]
        [ input [ onInput inputMsg, value query ] []
        , groups
            |> Dict.toList
            |> Fuzzy.simpleFilterItems query Tuple.first
            |> List.map
                (\( ( groupBase, ids ), textElement ) ->
                    p []
                        [ span
                            [ onClick (setGroupMsg <| sortIds words ids) ]
                            [ textElement
                            , text <| multiplier <| List.length ids
                            ]
                        , button
                            [ onClick <| addGroupMsg <| sortIds words ids ]
                            [ text "+" ]
                        ]
                )
            |> div []
        ]


multiplier : Int -> String
multiplier count =
    case count of
        1 ->
            ""

        n ->
            " (x" ++ toString n ++ ")"


sortIds : Dict String String -> List String -> List String
sortIds words ids =
    List.sortBy (\id -> Dict.get id words |> Maybe.withDefault "") ids
