module Url exposing (Url(..), parseLocation, path)

import UrlParser
import Navigation


type Url
    = Home
    | VideoList (List String)


parseLocation : Navigation.Location -> Url
parseLocation location =
    UrlParser.parseHash urlParser location
        |> Maybe.withDefault Home


urlParser : UrlParser.Parser (Url -> a) a
urlParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map (String.split "," >> VideoList) UrlParser.string
        ]


path : String -> Url -> String
path base url =
    case url of
        Home ->
            base

        VideoList ids ->
            base ++ "#" ++ String.join "," ids
