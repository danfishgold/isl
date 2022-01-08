module Colors exposing (SegmentedControl, black, block, blue, darkBlue, darkGreen, examples, green, linkHover, playbackRate, suggestions, title, variations, white)

import Element exposing (Color, rgb, rgb255)



-- https://paletton.com/#uid=53m0j0kqbCbfSNBlkHdz8zOzwoU


blue : Color
blue =
    rgb255 47 106 187


darkBlue : Color
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


block : { fill : Color, text : Color, hoverFill : Color, hoverText : Color }
block =
    { fill = green
    , text = white
    , hoverFill = darkGreen
    , hoverText = white
    }


title : { fill : Color, text : Color }
title =
    { fill = blue
    , text = white
    }


suggestions : { selectedFill : Color, selectedText : Color, unselectedFill : Color, unselectedText : Color, border : Color }
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


examples : { titleFill : Color, titleText : Color, hoverText : Color }
examples =
    { titleFill = green
    , titleText = white
    , hoverText = darkGreen
    }


linkHover : Color
linkHover =
    darkBlue
