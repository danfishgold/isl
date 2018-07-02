port module PlaybackRate exposing (set, setDelayed, control)

import Process
import Time exposing (second)
import Task
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)


port setPlaybackRate : Float -> Cmd msg


{-|
I need this because if I just use the port
whenever I add video elements to the page
the js code would run before they get rendered,
so I need to add some delay.
-}
setDelayed : (Float -> msg) -> Float -> Cmd msg
setDelayed toMsg rate =
    Process.sleep (0.03 * second)
        |> Task.andThen (always <| Task.succeed <| toMsg rate)
        |> Task.perform identity


set : Float -> Cmd msg
set =
    setPlaybackRate


control : (Float -> msg) -> Float -> Html msg
control toMsg currentRate =
    let
        rateButton rate =
            button
                [ onClick <| toMsg rate
                , disabled <| rate == currentRate
                ]
                [ text <| "x" ++ toString rate ]
    in
        text "מהירות"
            :: ([ 0.5, 0.75, 1 ]
                    |> List.map rateButton
               )
            |> div []
