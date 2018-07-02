module Main exposing (..)

import Navigation
import Html exposing (Html, div, input, p, h2, text, video)
import Html.Attributes exposing (dir, value, src, width, controls, autoplay, preload)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData, RemoteData(..))
import Dict exposing (Dict)
import Fuzzy
import Url exposing (Url)
import Dictionary exposing (Dictionary)
import PlaybackRate


-- MAIN


main : Program Bool Model Msg
main =
    Navigation.programWithFlags (Url.parseLocation >> UrlChange)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { dictionary : WebData Dictionary
    , query : String
    , selectedWords : List String
    , playbackRate : Float
    }



-- INIT


init : Bool -> Navigation.Location -> ( Model, Cmd Msg )
init isProduction location =
    ( { dictionary = NotAsked
      , query = ""
      , selectedWords =
            case Url.parseLocation location of
                Url.Home ->
                    []

                Url.VideoList ids ->
                    ids
      , playbackRate = 1
      }
    , Dictionary.get isProduction SetDictionary
    )



-- MSG


type Msg
    = SetDictionary (WebData Dictionary)
    | SetQuery String
    | ShowWords (List String)
    | SetPlaybackRate Float
    | UrlChange Url



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange url ->
            case url of
                Url.Home ->
                    ( { model
                        | query = ""
                        , selectedWords = []
                      }
                    , Cmd.none
                    )

                Url.VideoList ids ->
                    ( { model
                        | query = ""
                        , selectedWords = ids
                      }
                    , Cmd.none
                    )

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
            , Cmd.batch
                [ PlaybackRate.setDelayed SetPlaybackRate model.playbackRate
                , Navigation.newUrl <| Url.path <| Url.VideoList ids
                ]
            )

        SetPlaybackRate rate ->
            ( { model | playbackRate = rate }, PlaybackRate.set rate )


sortIds : Dict String String -> List String -> List String
sortIds words ids =
    List.sortBy (\id -> Dict.get id words |> Maybe.withDefault "") ids



-- VIEW


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
                    -- |> Fuzzy.filterItems 1 model.query Tuple.first
                    |>
                        Fuzzy.simpleFilterItems model.query Tuple.first
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
                    PlaybackRate.control SetPlaybackRate model.playbackRate
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
