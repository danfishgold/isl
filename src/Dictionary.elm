module Dictionary exposing (Dictionary, get)

import Dict exposing (Dict)
import Json.Decode as D
import RemoteData.Http
import RemoteData exposing (WebData)


type alias Dictionary =
    { words : Dict String String
    , groups : Dict String (List String)
    }


get : Bool -> (WebData Dictionary -> msg) -> Cmd msg
get isProduction toMsg =
    RemoteData.Http.get (dictionaryUrl isProduction) toMsg decoder


decoder : D.Decoder Dictionary
decoder =
    D.map2 Dictionary
        (D.field "words" <| D.dict D.string)
        (D.field "groups" <| D.dict <| D.list D.string)


dictionaryUrl : Bool -> String
dictionaryUrl isProduction =
    if isProduction then
        "http://files.fishgold.co/isl/combined.json"
    else
        "http://localhost:8000/combined.json"
