module SearchBar exposing (multiplier, sortIds, view)

import Dict exposing (Dict)
import Dictionary exposing (Dictionary)
import Fuzzy
import Html exposing (Html, button, div, input, p, span, text)
import Html.Attributes exposing (dir, value)
import Html.Events exposing (onClick, onInput)


view : (String -> msg) -> (List String -> msg) -> (List String -> msg) -> Dictionary -> String -> Html msg
view inputMsg setGroupMsg addGroupMsg { words, groups } query =
    div [ dir "rtl" ]
        [ input [ onInput inputMsg, value query ] []
        , groups
            |> Dict.toList
            |> Fuzzy.simpleFilterItems query Tuple.first
            |> List.map
                (\( ( _, ids ), textElement ) ->
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
            " (x" ++ String.fromInt n ++ ")"


sortIds : Dict String String -> List String -> List String
sortIds words ids =
    List.sortBy (\id -> Dict.get id words |> Maybe.withDefault "") ids
