module Route exposing (Route(..), parse, push, replace, toString)

import Browser.Navigation as Nav
import Dictionary exposing (WordId)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)
import Util exposing (maybeList)


type Route
    = Home
    | VideoList (List WordId)


parse : Url -> Route
parse url =
    Parser.parse parser url
        |> Maybe.withDefault Home


parser : Parser (Route -> a) a
parser =
    Parser.top </> Parser.fragment fragmentParser


fragmentParser : Maybe String -> Route
fragmentParser frag =
    case frag of
        Nothing ->
            Home

        Just str ->
            Dictionary.wordIdsFromSlug str
                |> Maybe.map VideoList
                |> Maybe.withDefault Home


toString : Route -> String
toString route =
    case route of
        Home ->
            "#"

        VideoList ids ->
            "#" ++ Dictionary.wordIdsToSlug ids


push : Nav.Key -> Route -> Cmd msg
push key route =
    Nav.pushUrl key (toString route)


replace : Nav.Key -> Route -> Cmd msg
replace key route =
    Nav.replaceUrl key (toString route)
