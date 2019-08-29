module Dictionary exposing
    ( Dictionary
    , WordId
    , fetch
    , group
    , primaryWordList
    , title
    , wordIdToString
    , wordIdsFromSlug
    , wordIdsToSlug
    )

import Base64
import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (WebData)
import RemoteData.Http
import Util exposing (bytesDecodeList, maybeList)


type Dictionary
    = Dictionary
        { words : Dict Int String
        , groups : Dict String Group
        }


type alias Group =
    { primary : WordId
    , variations : List WordId
    }


type WordId
    = WordId Int



-- ACCESS


primaryWordList : Dictionary -> List WordId
primaryWordList (Dictionary { groups }) =
    Dict.values groups |> List.map .primary


title : Dictionary -> WordId -> String
title (Dictionary { words }) (WordId id) =
    Dict.get id words |> Maybe.withDefault "מילה לא מוכרת"


group : Dictionary -> WordId -> Group
group ((Dictionary { groups }) as dict) wordId =
    Dict.get (title dict wordId) groups
        |> Maybe.withDefault { primary = wordId, variations = [] }



-- DECODING


fetch : (WebData Dictionary -> msg) -> Cmd msg
fetch toMsg =
    RemoteData.Http.get "dictionary.json" toMsg decoder


decoder : Decoder Dictionary
decoder =
    Decode.map2 (\words groups -> Dictionary { words = words, groups = groups })
        (Decode.field "words" <| Decode.andThen intDictFromStringDict <| Decode.dict Decode.string)
        (Decode.field "groups" <| Decode.dict <| Decode.andThen groupDecoder <| Decode.list wordIdDecoder)


groupDecoder : List WordId -> Decoder Group
groupDecoder ids =
    case ids of
        [] ->
            Decode.fail "empty group"

        hd :: tl ->
            Decode.succeed { primary = hd, variations = tl }


wordIdDecoder : Decoder WordId
wordIdDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case String.toInt str of
                    Just int ->
                        Decode.succeed (WordId int)

                    Nothing ->
                        Decode.fail "not an integer"
            )


wordIdToString : WordId -> String
wordIdToString (WordId id) =
    String.fromInt id


intDictFromStringDict : Dict String value -> Decoder (Dict Int value)
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
            Decode.fail "Couldn't parse word dictionary"

        Just coolDict ->
            Decode.succeed coolDict



-- URL SLUGS


wordIdBytesEncoder : WordId -> BE.Encoder
wordIdBytesEncoder (WordId id) =
    BE.unsignedInt16 Bytes.LE id


wordIdBytesDecoder : BD.Decoder WordId
wordIdBytesDecoder =
    BD.unsignedInt16 Bytes.LE |> BD.map WordId


wordIdsToSlug : List WordId -> String
wordIdsToSlug wordIds =
    wordIds
        |> List.map wordIdBytesEncoder
        |> BE.sequence
        |> BE.encode
        |> Base64.fromBytes
        |> Maybe.withDefault ""
        |> String.replace "=" ""


wordIdsFromSlug : String -> Maybe (List WordId)
wordIdsFromSlug slug =
    Base64.toBytes slug
        |> Maybe.andThen
            (\bytes ->
                bytesDecodeList
                    (Bytes.width bytes // 2)
                    wordIdBytesDecoder
                    bytes
            )
