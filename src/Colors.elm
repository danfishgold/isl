module Colors exposing (SegmentedControl, block, examples, languageHover, playbackRate, suggestions, title, variations)

import Element exposing (Color, rgb, rgb255)



-- https://paletton.com/#uid=53m0j0kqbCbfSNBlkHdz8zOzwoU


turquoise : Color
turquoise =
    rgb255 36 155 179


darkTurtuiose : Color
darkTurtuiose =
    rgb255 15 130 153


blue : Color
blue =
    rgb255 47 106 187


darkBlue =
    rgb255 24 82 161


green : Color
green =
    rgb255 35 193 125


darkGreen : Color
darkGreen =
    rgb255 13 168 102


white : Color
white =
    rgb 1 1 1


black : Color
black =
    rgb 0 0 0


lightGray : Color
lightGray =
    rgb 0.9 0.9 0.9


gray : Color
gray =
    rgb 0.8 0.8 0.8


block =
    { fill = green
    , text = white
    , hoverFill = darkGreen
    , hoverText = white
    }


title =
    { fill = blue
    , text = white
    }


suggestions =
    { selectedFill = green
    , selectedText = white
    , unselectedFill = lightGray
    , unselectedText = black
    , border = black
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
    { selectedFill = blue
    , selectedText = white
    , hoverFill = gray
    , hoverText = black
    , backgroundFill = lightGray
    }


variations : SegmentedControl
variations =
    { selectedFill = blue
    , selectedText = white
    , hoverFill = gray
    , hoverText = black
    , backgroundFill = lightGray
    }


examples =
    { titleFill = green
    , titleText = white
    , hoverText = darkGreen
    }


languageHover =
    darkBlue
