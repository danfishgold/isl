module BlockBar exposing (blockBar)

-- import Element exposing (..)

import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
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


block : msg -> String -> Element msg
block removeBlock title =
    el
        [ Background.color Colors.block.fill
        , Border.rounded 5
        , paddingXY 10 5
        , Font.color Colors.block.text
        , Font.bold
        , onClick removeBlock
        , mouseOver
            [ Background.color Colors.block.hoverFill
            , Font.color Colors.block.hoverText
            ]
        ]
        (text title)


blockBar : (Key -> msg) -> (String -> msg) -> (Int -> msg) -> (block -> String) -> Query block -> List (Element.Attribute msg) -> Element msg
blockBar onKey onChange removeBlock blockToString query attrs =
    let
        placeholder =
            if Query.hasBlocks query then
                Nothing

            else
                Just "לך חפש"
    in
    wrappedRow
        ([ Border.solid
         , Border.width 1
         , Border.rounded 3
         , padding 5
         , spacing 5
         ]
            ++ attrs
        )
        (List.concat
            [ Query.blockList query |> List.indexedMap (\idx block_ -> block (removeBlock idx) (blockToString block_))
            , [ input onKey onChange placeholder (Query.text query) ]
            ]
        )
