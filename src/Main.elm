port module Main exposing (..)

import Html exposing (Html, program, div, input, p, h2, text, video, button)
import Html.Attributes exposing (dir, value, src, width, controls, autoplay, preload, disabled)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData, RemoteData(..))
import RemoteData.Http
import Dict exposing (Dict)
import Json.Decode as D
import Fuzzy
import Process
import Time exposing (second)
import Task


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
    , playbackRate : Float
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
      , playbackRate = 1
      }
    , RemoteData.Http.get "http://localhost:8000/combined.json" SetDictionary dictionaryDecoder
    )


type Msg
    = SetDictionary (WebData Dictionary)
    | SetQuery String
    | ShowWords (List String)
    | SetPlaybackRate Float


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port setPlaybackRate : Float -> Cmd msg


{-|
I need this because if I just use the port
whenever I add video elements to the page
the js code would run before they get rendered,
so I need to add some delay.
-}
delayedSetPlaybackRate : Float -> Cmd Msg
delayedSetPlaybackRate rate =
    Process.sleep (0.03 * second)
        |> Task.andThen (always <| Task.succeed <| SetPlaybackRate rate)
        |> Task.perform identity


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
            , delayedSetPlaybackRate model.playbackRate
            )

        SetPlaybackRate rate ->
            ( { model | playbackRate = rate }, setPlaybackRate rate )


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
                [ text "באופן זמני לא כל המילים יעבדו"
                , input [ onInput SetQuery, value model.query ] []
                , groups
                    |> Dict.toList
                    |> Fuzzy.filterItems 1 model.query Tuple.first
                    |> List.map
                        (\( ( groupBase, ids ), textElement ) ->
                            p
                                [ onClick (ShowWords <| sortIds words ids) ]
                                [ textElement
                                , text <| multiplier <| List.length ids
                                ]
                        )
                    |> div []
                , if List.length model.selectedWords > 0 then
                    playbackRateControl model.playbackRate
                  else
                    text ""
                , model.selectedWords
                    |> List.filterMap (\k -> Dict.get k words |> Maybe.map (\w -> ( k, w )))
                    |> List.map (\( id, word ) -> video id word)
                    |> div []
                ]

        Failure error ->
            div []
                [ text "Error: "
                , text <| toString error
                ]


multiplier : Int -> String
multiplier count =
    case count of
        1 ->
            ""

        n ->
            " (x" ++ toString n ++ ")"


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


playbackRateControl : Float -> Html Msg
playbackRateControl currentRate =
    let
        rateButton rate =
            button
                [ onClick <| SetPlaybackRate rate
                , disabled <| rate == currentRate
                ]
                [ text <| "x" ++ toString rate ]
    in
        text "מהירות"
            :: ([ 0.5, 0.75, 1 ]
                    |> List.map rateButton
               )
            |> div []
