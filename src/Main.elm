module Main exposing (..)

import Html exposing (Html, program, div, input, p, h2, text, video, source)
import Html.Keyed
import Html.Attributes exposing (dir, value, src, width, controls, autoplay, preload)
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
    { words : Dict String String
    , groups : Dict String (List String)
    }


dictionaryDecoder : D.Decoder Dictionary
dictionaryDecoder =
    D.map2 Dictionary
        (D.field "words" <| D.dict D.string)
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


sortIds : Dict String String -> List String -> List String
sortIds words ids =
    List.sortBy (\id -> Dict.get id words |> Maybe.withDefault "") ids


view : Model -> Html Msg
view model =
    case model.dictionary of
        NotAsked ->
            text "Loading..."

        Loading ->
            text "Loading..."

        Success { words, groups } ->
            div [ dir "rtl" ]
                [ input [ onInput SetQuery, value model.query ] []
                , groups
                    |> Dict.toList
                    |> Fuzzy.filterItems 1 model.query Tuple.first
                    |> List.map
                        (\( groupBase, ids ) ->
                            p
                                [ onClick (ShowWords <| sortIds words ids) ]
                                [ text <| groupTitle groupBase ids ]
                        )
                    |> div []
                , model.selectedWords
                    |> List.filterMap (\k -> Dict.get k words |> Maybe.map (\w -> ( k, w )))
                    |> List.map (\( id, word ) -> ( "video-" ++ word, video id word ))
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


video : String -> String -> Html msg
video id word =
    div []
        [ Html.h2 [] [ text word ]
        , Html.video
            [ src <| "http://files.fishgold.co/isl/videos/" ++ id ++ ".mp4"
            , width 400
            , controls True
            , autoplay True
            , preload "auto"
            ]
            []
        ]
