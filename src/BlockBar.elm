module BlockBar exposing (blockBar)

import Dictionary exposing (Dictionary)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Query exposing (Query)


keyDecoder : String -> Decoder KeyEvent
keyDecoder key =
    if key == "Enter" then
        Decode.succeed Enter

    else if key == "Backspace" then
        Decode.succeed Backspace

    else
        Decode.fail "Not the enter key"


type KeyEvent
    = Enter
    | Backspace
    | Character String


mapKeyEvent : msg -> (Query -> msg) -> Query -> KeyEvent -> msg
mapKeyEvent onEnter onUpdatedQuery query keyEvent =
    case keyEvent of
        Enter ->
            if String.isEmpty query.text then
                onEnter

            else
                onUpdatedQuery
                    { query
                        | text = ""
                        , blocksBefore = query.blocksBefore ++ [ query.text ]
                    }

        Backspace ->
            if String.isEmpty query.text then
                onUpdatedQuery { query | blocksBefore = removeLast query.blocksBefore }

            else
                onUpdatedQuery { query | text = removeLastChar query.text }

        Character char ->
            onUpdatedQuery { query | text = query.text ++ char }


removeLast : List a -> List a
removeLast xs =
    List.take (List.length xs - 1) xs


removeLastChar : String -> String
removeLastChar str =
    String.dropRight 1 str


input : msg -> (Query -> msg) -> Query -> Element msg
input onEnter onQueryUpdate query =
    Input.text
        [ htmlAttribute <|
            Html.Events.on "keyup"
                (Decode.field "key"
                    (Decode.string
                        |> Decode.andThen keyDecoder
                        |> Decode.map (mapKeyEvent onEnter onQueryUpdate query)
                    )
                )
        , htmlAttribute <|
            Html.Events.on "input"
                (Decode.field "data" Decode.string
                    |> Decode.map Character
                    |> Decode.map (mapKeyEvent onEnter onQueryUpdate query)
                )
        , Input.focusedOnLoad
        , Border.width 0
        , padding 5
        , focused []
        ]
        { label = Input.labelHidden ""
        , onChange = always onEnter
        , placeholder =
            if Query.isEmpty query then
                Just (Input.placeholder [] <| text "לך חפש")

            else
                Nothing
        , text = query.text
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


blockBar : msg -> (Query -> msg) -> Query -> Element msg
blockBar onEnter onQueryUpdate query =
    wrappedRow
        [ Border.solid
        , Border.width 2
        , padding 5
        , spacing 5
        ]
        (List.concat
            [ query.blocksBefore |> List.map block
            , [ input onEnter onQueryUpdate query ]
            ]
        )
