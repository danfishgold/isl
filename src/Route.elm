module Route exposing (Route(..), parse, push, replace, toString)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)
import Util exposing (maybeList)


type Route
    = Home
    | VideoList (List Int)


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
            listOfInts str
                |> Maybe.map VideoList
                |> Maybe.withDefault Home


listOfInts : String -> Maybe (List Int)
listOfInts str =
    str
        |> String.split ","
        |> List.map String.toInt
        |> maybeList


toString : Route -> String
toString route =
    case route of
        Home ->
            "#"

        VideoList ids ->
            "#" ++ String.join "," (List.map String.fromInt ids)


push : Nav.Key -> Route -> Cmd msg
push key route =
    Nav.pushUrl key (toString route)


replace : Nav.Key -> Route -> Cmd msg
replace key route =
    Nav.replaceUrl key (toString route)
