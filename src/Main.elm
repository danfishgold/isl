module Main exposing (..)

import Html exposing (Html, program, div, input, p, h2, text, video, source)
import Html.Keyed
import Html.Attributes exposing (dir, src, type_, width)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData, RemoteData(..))
import RemoteData.Http
import Dict exposing (Dict)
import Json.Decode as D
import Fuzzy


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { dictionary : WebData Dictionary
    , query : String
    , selectedWords : List String
    }


type alias Dictionary =
    { words : Dict String Word
    , groups : Dict String (List String)
    }


type alias Word =
    { text : String
    , sources : Dict String String
    }


wordsDecoder : D.Decoder (Dict String Word)
wordsDecoder =
    D.map2 Word
        (D.field "text" D.string)
        (D.field "sources" <| D.dict D.string)
        |> D.dict


dictionaryDecoder : D.Decoder Dictionary
dictionaryDecoder =
    D.map2 Dictionary
        (D.field "words" wordsDecoder)
        (D.field "groups" <| D.dict <| D.list D.string)


init : ( Model, Cmd Msg )
init =
    ( { dictionary = NotAsked
      , query = ""
      , selectedWords = []
      }
    , RemoteData.Http.get "http://localhost:8000/combined.json" SetDictionary dictionaryDecoder
    )


type Msg
    = SetDictionary (WebData Dictionary)
    | SetQuery String
    | ShowWords (List String)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDictionary dictionary ->
            ( { model | dictionary = dictionary }, Cmd.none )

        SetQuery q ->
            ( { model
                | query = q
                , selectedWords = []
              }
            , Cmd.none
            )

        ShowWords ids ->
            ( { model
                | selectedWords = ids
                , query = ""
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.dictionary of
        NotAsked ->
            text "Loading..."

        Loading ->
            text "Loading..."

        Success { words, groups } ->
            div [ dir "rtl" ]
                [ input [ onInput SetQuery ] []
                , groups
                    |> Dict.toList
                    |> Fuzzy.filterItems 1 model.query Tuple.first
                    |> List.map
                        (\( groupBase, ids ) ->
                            p
                                [ onClick (ShowWords ids) ]
                                [ text <| groupTitle groupBase ids ]
                        )
                    |> div []
                , model.selectedWords
                    |> List.filterMap (\k -> Dict.get k words)
                    |> List.map (\word -> ( "video-" ++ word.text, video word ))
                    |> Html.Keyed.node "div" []
                ]

        Failure error ->
            div []
                [ text "Error: "
                , text <| toString error
                ]


groupTitle : String -> List String -> String
groupTitle groupBase ids =
    case List.length ids of
        1 ->
            groupBase

        n ->
            groupBase ++ " (x" ++ toString n ++ ")"


video : Word -> Html msg
video word =
    div []
        [ Html.h2 [] [ text word.text ]
        , word.sources
            |> Dict.toList
            |> List.map (\( mediaType, url ) -> source [ src url, type_ mediaType ] [])
            |> Html.video [ Html.Attributes.width 400, Html.Attributes.controls True ]
        ]
