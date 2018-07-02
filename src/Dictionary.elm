module Dictionary exposing (Dictionary, get)

import Dict exposing (Dict)
import Json.Decode as D
import RemoteData.Http
import RemoteData exposing (WebData)


type alias Dictionary =
    { words : Dict String String
    , groups : Dict String (List String)
    }


get : String -> (WebData Dictionary -> msg) -> Cmd msg
get baseUrl toMsg =
    RemoteData.Http.get (dictionaryUrl baseUrl) toMsg decoder


decoder : D.Decoder Dictionary
decoder =
    D.map2 Dictionary
        (D.field "words" <| D.dict D.string)
        (D.field "groups" <| D.dict <| D.list D.string)


dictionaryUrl : String -> String
dictionaryUrl baseUrl =
    baseUrl ++ "combined.json"
