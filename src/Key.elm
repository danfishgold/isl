module Key exposing (Key(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Key
    = Enter
    | Backspace
    | Up
    | Down


decoder : String -> Decoder Key
decoder key =
    case key of
        "Enter" ->
            Decode.succeed Enter

        "Backspace" ->
            Decode.succeed Backspace

        "UpArrow" ->
            Decode.succeed Up

        "DownArrow" ->
            Decode.succeed Down

        _ ->
            Decode.fail "Nah"
