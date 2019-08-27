module Dictionary exposing (Dictionary, get)

import Dict exposing (Dict)
import Json.Decode as D
import RemoteData exposing (WebData)
import RemoteData.Http


type alias Dictionary =
    { words : Dict String String
    , groups : Dict String (List String)
    }


get : (WebData Dictionary -> msg) -> Cmd msg
get toMsg =
    RemoteData.Http.get "combined.json" toMsg decoder


decoder : D.Decoder Dictionary
decoder =
    D.map2 Dictionary
        (D.field "words" <| D.dict D.string)
        (D.field "groups" <| D.dict <| D.list D.string)
