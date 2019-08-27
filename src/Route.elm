module Route exposing (Route(..), parse, push, toString)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home
    | VideoList (List String)


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
            VideoList (String.split "," str)


toString : Route -> String
toString route =
    case route of
        Home ->
            "#"

        VideoList ids ->
            "#" ++ String.join "," ids


push : Nav.Key -> Route -> Cmd msg
push key route =
    Nav.pushUrl key (toString route)
