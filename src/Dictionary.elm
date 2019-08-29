module Dictionary exposing
    ( Dictionary
    , WordId
    , fetch
    , groupList
    , primaryWordList
    , title
    , wordIdToString
    , wordIdsFromString
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (WebData)
import RemoteData.Http
import Util exposing (maybeList)


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


groupList : Dictionary -> List ( String, Group )
groupList (Dictionary { groups }) =
    Dict.toList groups


primaryWordList : Dictionary -> List WordId
primaryWordList (Dictionary { groups }) =
    Dict.values groups |> List.map .primary


title : Dictionary -> WordId -> String
title (Dictionary { words }) (WordId id) =
    Dict.get id words |> Maybe.withDefault "מילה לא מוכרת"


primaryWordForGroup : String -> Dictionary -> Maybe WordId
primaryWordForGroup group (Dictionary { groups }) =
    Dict.get group groups |> Maybe.map .primary



-- DECODING


fetch : (WebData Dictionary -> msg) -> Cmd msg
fetch toMsg =
    RemoteData.Http.get "new_combined.json" toMsg decoder


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


wordIdsFromString : String -> Maybe (List WordId)
wordIdsFromString str =
    str
        |> String.split ","
        |> List.map String.toInt
        |> maybeList
        |> Maybe.map (List.map WordId)


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
