module Colors exposing (SegmentedControl, block, playbackRate, suggestions, variations)

import Element exposing (Color, rgb)


block =
    { fill = rgb 1 0 0
    , text = rgb 1 1 1
    , hoverFill = rgb 0.9 0 0
    , hoverText = rgb 1 1 1
    }


suggestions =
    { selectedFill = rgb 1 0 0
    , selectedText = rgb 1 1 1
    , unselectedFill = rgb 1 1 1
    , unselectedText = rgb 0 0 0
    }


type alias SegmentedControl =
    { selectedFill : Color
    , selectedText : Color
    , hoverFill : Color
    , hoverText : Color
    , backgroundFill : Color
    }


playbackRate : SegmentedControl
playbackRate =
    { selectedFill = rgb 1 0 0
    , selectedText = rgb 1 1 1
    , hoverFill = rgb 1 0.9 0.9
    , hoverText = rgb 0 0 0
    , backgroundFill = rgb 0.9 0.9 0.9
    }


variations : SegmentedControl
variations =
    { selectedFill = rgb 1 0 0
    , selectedText = rgb 1 1 1
    , hoverFill = rgb 1 0.9 0.9
    , hoverText = rgb 0 0 0
    , backgroundFill = rgb 0.9 0.9 0.9
    }
