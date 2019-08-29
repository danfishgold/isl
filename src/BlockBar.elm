module BlockBar exposing (blockBar)

-- import Element exposing (..)

import Element
    exposing
        ( Element
        , el
        , focused
        , htmlAttribute
        , padding
        , paddingXY
        , rgb
        , spacing
        , text
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Json.Decode as Decode
import Key exposing (Key)
import Query exposing (Query)


input : (Key -> msg) -> (String -> msg) -> Maybe String -> String -> Element msg
input onKey onChange placeholder queryText =
    Input.text
        [ htmlAttribute <|
            Html.Events.on "keydown"
                (Decode.field "key"
                    (Decode.string
                        |> Decode.andThen Key.decoder
                        |> Decode.map onKey
                    )
                )
        , Input.focusedOnLoad
        , Border.width 0
        , padding 5
        , focused []
        ]
        { label = Input.labelHidden ""
        , onChange = onChange
        , placeholder = placeholder |> Maybe.map (text >> Input.placeholder [])
        , text = queryText
        }


block : String -> Element msg
block title =
    el
        [ Background.color (rgb 0.8 0 0)
        , Border.rounded 5
        , paddingXY 10 5
        , Font.color (rgb 1 1 1)
        , Font.bold
        ]
        (text title)


blockBar : (Key -> msg) -> (String -> msg) -> (block -> String) -> Query block -> Element msg
blockBar onKey onChange blockToString query =
    let
        placeholder =
            if Query.hasBlocks query then
                Nothing

            else
                Just "לך חפש"
    in
    wrappedRow
        [ Border.solid
        , Border.width 2
        , padding 5
        , spacing 5
        ]
        (List.concat
            [ Query.blockList query |> List.map (blockToString >> block)
            , [ input onKey onChange placeholder query.text ]
            ]
        )
