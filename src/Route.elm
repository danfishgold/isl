module Route exposing (Route(..), default, parse, push, replace, toString)

import Browser.Navigation as Nav
import Dictionary exposing (WordId)
import Localization as L10n exposing (Locale(..))
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = VideoList Locale (List WordId)
    | Slide Int


default =
    VideoList Hebrew []


parse : Url -> Maybe Route
parse url =
    Parser.parse parser url |> Maybe.andThen identity


parser : Parser (Maybe Route -> a) a
parser =
    Parser.top </> Parser.fragment (Maybe.withDefault "" >> fragmentParser)


fragmentParser : String -> Maybe Route
fragmentParser frag =
    if String.startsWith "slide" frag then
        frag
            |> String.dropLeft 5
            |> String.toInt
            |> Maybe.map (\n -> n - 1)
            |> Maybe.map Slide

    else
        case String.split "-" frag of
            [ localeString ] ->
                L10n.localeFromString localeString |> Maybe.map (\locale -> VideoList locale [])

            [ localeString, encodedWordIds ] ->
                Maybe.map2 VideoList
                    (L10n.localeFromString localeString)
                    (Dictionary.wordIdsFromSlug encodedWordIds)

            _ ->
                Nothing


toString : Route -> String
toString route =
    case route of
        VideoList locale [] ->
            "#" ++ L10n.localeToString locale

        VideoList locale ids ->
            "#" ++ L10n.localeToString locale ++ "-" ++ Dictionary.wordIdsToSlug ids

        Slide n ->
            "#slide" ++ String.fromInt (n + 1)


push : Nav.Key -> Route -> Cmd msg
push key route =
    Nav.pushUrl key (toString route)


replace : Nav.Key -> Route -> Cmd msg
replace key route =
    Nav.replaceUrl key (toString route)
