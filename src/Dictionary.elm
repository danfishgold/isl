module Dictionary exposing (Dictionary, get)

import Dict exposing (Dict)
import Json.Decode as D
import RemoteData exposing (WebData)
import RemoteData.Http
import Util exposing (maybeList)


type alias Dictionary =
    { words : Dict Int String
    , groups : Dict String (List Int)
    }


get : (WebData Dictionary -> msg) -> Cmd msg
get toMsg =
    RemoteData.Http.get "combined.json" toMsg decoder


decoder : D.Decoder Dictionary
decoder =
    D.map2 Dictionary
        (D.field "words" <| D.dict D.int)
        (D.field "groups" <| D.andThen intDictFromStringDict <| D.dict (D.list D.string))


intDictFromStringDict : Dict String value -> D.Decoder (Dict Int value)
intDictFromStringDict dict =
    let
        maybeDict =
            dict
                |> Dict.toList
                |> List.map
                    (\( k, val ) ->
                        String.toInt k |> Maybe.map (\intK -> ( intK, val ))
                    )
                |> maybeList
                |> Maybe.map Dict.fromList
    in
    case maybeDict of
        Nothing ->
            D.fail "Couldn't parse word dictionary"

        Just coolDict ->
            D.succeed coolDict
