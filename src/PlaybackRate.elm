port module PlaybackRate exposing (control, set, setDelayed)

import Colors
import Element exposing (..)
import Localization as L10n exposing (Locale)
import Process
import Task
import Util exposing (segmentedControl)


port setPlaybackRate : Float -> Cmd msg


{-| I need this because if I just use the port
whenever I add video elements to the page
the js code would run before they get rendered,
so I need to add some delay.
-}
setDelayed : (Float -> msg) -> Float -> Cmd msg
setDelayed toMsg rate =
    Process.sleep (0.03 * 1000)
        |> Task.andThen (always <| Task.succeed <| toMsg rate)
        |> Task.perform identity


set : Float -> Cmd msg
set =
    setPlaybackRate


control : Locale -> (Float -> msg) -> Float -> Element msg
control locale toMsg currentRate =
    [ text "מהירות"
    , [ 0.5, 0.75, 1 ]
        |> List.map (\rate -> ( rate, "x" ++ String.fromFloat rate ))
        |> segmentedControl Colors.playbackRate toMsg currentRate
    ]
        |> L10n.container locale row [ spacing 10 ]
