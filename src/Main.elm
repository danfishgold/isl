module Main exposing (..)

import Navigation
import Html exposing (Html, div, input, p, h2, text, button, video)
import Html.Keyed exposing (node)
import Html.Attributes exposing (dir, value, src, width, controls, autoplay, preload, style)
import Html.Events exposing (onClick)
import RemoteData exposing (WebData, RemoteData(..))
import Dict exposing (Dict)
import Url exposing (Url)
import Dictionary exposing (Dictionary)
import PlaybackRate
import SearchBar


-- MAIN


main : Program String Model Msg
main =
    Navigation.programWithFlags (Url.parseLocation >> UrlChange)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { baseUrl : String
    , dictionary : WebData Dictionary
    , query : String
    , selectedWords : List String
    , playbackRate : Float
    }



-- INIT


init : String -> Navigation.Location -> ( Model, Cmd Msg )
init baseUrl location =
    ( { baseUrl = baseUrl
      , dictionary = NotAsked
      , query = ""
      , selectedWords =
            case Url.parseLocation location of
                Url.Home ->
                    []

                Url.VideoList ids ->
                    ids
      , playbackRate = 1
      }
    , Dictionary.get baseUrl SetDictionary
    )



-- MSG


type Msg
    = SetDictionary (WebData Dictionary)
    | SetQuery String
    | ShowWords (List String)
    | RemoveWord Int
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
            ( { model | query = q }
            , Cmd.none
            )

        ShowWords ids ->
            ( { model
                | selectedWords = ids
                , query = ""
              }
            , Cmd.batch
                [ PlaybackRate.setDelayed SetPlaybackRate model.playbackRate
                , Navigation.newUrl <| Url.path model.baseUrl <| Url.VideoList ids
                ]
            )

        RemoveWord idx ->
            let
                newIds =
                    listRemove idx model.selectedWords
            in
                ( { model | selectedWords = newIds }
                , Navigation.newUrl <| Url.path model.baseUrl <| Url.VideoList newIds
                )

        SetPlaybackRate rate ->
            ( { model | playbackRate = rate }, PlaybackRate.set rate )


listRemove : Int -> List a -> List a
listRemove idx list =
    List.take (idx) list ++ List.drop (idx + 1) list



-- VIEW


view : Model -> Html Msg
view model =
    case model.dictionary of
        NotAsked ->
            text "Loading..."

        Loading ->
            text "Loading..."

        Success dict ->
            div [ dir "rtl" ]
                [ SearchBar.view
                    SetQuery
                    ShowWords
                    (\newIds -> ShowWords (model.selectedWords ++ newIds))
                    dict
                    model.query
                , if List.length model.selectedWords > 0 then
                    PlaybackRate.control SetPlaybackRate model.playbackRate
                  else
                    text ""
                , model.selectedWords
                    |> List.filterMap (\k -> Dict.get k dict.words |> Maybe.map (\w -> ( k, w )))
                    |> List.indexedMap (\idx ( id, word ) -> ( id, video id word (RemoveWord idx) ))
                    |> node "div" [ style [ ( "display", "flex" ), ( "flex-wrap", "wrap" ) ] ]
                ]

        Failure error ->
            div []
                [ text "Error: "
                , text <| toString error
                ]


video : String -> String -> msg -> Html msg
video id word removeMsg =
    div []
        [ div [ style [ ( "display", "flex" ), ( "align-items", "baseline" ) ] ]
            [ h2 [] [ text word ]
            , button [ onClick removeMsg ] [ text "מחק" ]
            ]
        , Html.video
            [ src <| "http://files.fishgold.co/isl/videos/" ++ id ++ ".mp4"
            , controls True
            , autoplay True
            , Html.Attributes.attribute "muted" "true"
            , Html.Attributes.attribute "playsinline" "true"
            , preload "auto"
            ]
            []
        ]
