module Slides exposing (view)

import Array exposing (Array)
import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font


view : (Int -> msg) -> Int -> Element msg
view setSlideNumber slideNumber =
    Array.get slideNumber slides
        |> Maybe.withDefault []
        |> List.map render
        |> column
            [ width fill
            , height fill
            , spacing 5
            , behindContent <|
                row [ width fill, height fill ]
                    [ el
                        [ width (fillPortion 1)
                        , height fill
                        , onClick <| setSlideNumber <| max 0 <| slideNumber - 1
                        ]
                        Element.none
                    , el
                        [ width (fillPortion 2)
                        , height fill
                        , onClick <| setSlideNumber <| min (Array.length slides - 1) <| slideNumber + 1
                        ]
                        Element.none
                    ]
            ]


slides : Array (List Object)
slides =
    [ progressive
        [ Title "Hey"
        , Text "I'm Dan Fishgold"
        , Text "I live in Israel"
        , Text "About a year ago I took an introductory course in Israeli Sign Language (ISL)"
        , Link "I made this website" "#he"
        ]
    , progressive
        [ Title "Sign Languages"
        , Text "Different in every country/region"
        , Text "Not a 1:1 mapping"
        , Text "Real languages"
        ]
    , progressive
        [ Title "Fun Facts"
        , Text "Tone"
        , Link "Variations based on context" "#en-SRZVFlkWARYFFg"
        , Link "Etymology" "#en-RSAdDO0tsQs1Krkn"
        ]
    , single
        [ Title "Talk to me About Stuff"
        , Text "ISL"
        , Text "Linguistics"
        , Text "Glitch"
        , Text "Burritos"
        , Text "Podcasts"
        , Text "Elm"
        , Text "Anything"
        ]
    , single [ Title "Thanks" ]
    ]
        |> List.concat
        |> Array.fromList


single : List a -> List (List a)
single slide =
    [ slide ]


progressive : List a -> List (List a)
progressive slide =
    case slide of
        [] ->
            []

        title :: points ->
            points
                |> List.length
                |> List.range 0
                |> List.map (\n -> title :: List.take n points)


type Object
    = Title String
    | Text String
    | Link String String


render : Object -> Element msg
render object =
    case object of
        Title content ->
            el [ padding 30, centerX ] <|
                paragraph
                    [ padding 10
                    , Border.rounded 5
                    , Background.color Colors.blue
                    , Font.color Colors.white
                    , Font.bold
                    , Font.size 48
                    ]
                    [ text content ]

        Text content ->
            paragraph
                [ centerX
                , width shrink
                , height shrink
                , padding 5
                , Font.alignLeft
                , Font.size 30
                , width (maximum 700 fill)
                ]
                [ text content ]

        Link content url ->
            paragraph
                [ centerX
                , width shrink
                , height shrink
                , padding 5
                , Font.alignLeft
                , Font.size 30
                , width (maximum 700 fill)
                ]
                [ link [ Font.underline ]
                    { label = text content, url = url }
                ]
