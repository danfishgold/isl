module Video exposing (element)

import Dictionary exposing (WordId)
import Element exposing (Element, html)
import Html
import Html.Attributes exposing (attribute, autoplay, controls, preload, src)


element : WordId -> Element msg
element wordId =
    Element.html <|
        Html.video
            [ src <| "https://files.fishgold.co.il/isl/videos/" ++ Dictionary.wordIdToString wordId ++ ".mp4"
            , controls True
            , autoplay True
            , attribute "muted" "true"
            , attribute "playsinline" "true"
            , preload "auto"
            ]
            []
