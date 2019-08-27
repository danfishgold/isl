module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Dictionary exposing (Dictionary)
import Html exposing (Html, button, div, h2, text, video)
import Html.Attributes exposing (autoplay, controls, dir, preload, src, style)
import Html.Events exposing (onClick)
import Html.Keyed exposing (node)
import PlaybackRate
import RemoteData exposing (RemoteData(..), WebData)
import Route
import SearchBar
import Url exposing (Url)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { dictionary : WebData Dictionary
    , query : String
    , selectedWords : List String
    , playbackRate : Float
    , key : Nav.Key
    }



-- MSG


type Msg
    = SetDictionary (WebData Dictionary)
    | SetQuery String
    | ShowWords (List String)
    | RemoveWord Int
    | SetPlaybackRate Float
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest



-- INIT


type alias Flags =
    ()


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        ( model, pageCmd ) =
            updateModelWithUrl url
                { dictionary = Loading
                , query = ""
                , selectedWords = []
                , playbackRate = 1
                , key = key
                }
    in
    ( model, Cmd.batch [ pageCmd, Dictionary.get SetDictionary ] )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            updateModelWithUrl url model

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
                , Route.push model.key (Route.VideoList ids)
                ]
            )

        RemoveWord idx ->
            let
                newIds =
                    listRemove idx model.selectedWords
            in
            ( { model | selectedWords = newIds }
            , Route.push model.key (Route.VideoList newIds)
            )

        SetPlaybackRate rate ->
            ( { model | playbackRate = rate }, PlaybackRate.set rate )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


updateModelWithUrl : Url -> Model -> ( Model, Cmd Msg )
updateModelWithUrl url model =
    case Route.parse url of
        Route.Home ->
            ( { model
                | query = ""
                , selectedWords = []
              }
            , Cmd.none
            )

        Route.VideoList ids ->
            ( { model
                | query = ""
                , selectedWords = ids
              }
            , Cmd.none
            )


listRemove : Int -> List a -> List a
listRemove idx list =
    List.take idx list ++ List.drop (idx + 1) list



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "מילון שפת הסימנים"
    , body = [ body model ]
    }


body : Model -> Html Msg
body model =
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
                    |> node "div" [ style "display" "flex", style "flex-wrap" "wrap" ]
                ]

        Failure _ ->
            div []
                [ text "אוי לא! היתה שגיאת רשת"
                ]


video : String -> String -> msg -> Html msg
video id word removeMsg =
    div []
        [ div [ style "display" "flex", style "align-items" "baseline" ]
            [ h2 [] [ text word ]
            , button [ onClick removeMsg ] [ text "מחק" ]
            ]
        , Html.video
            [ src <| "http://files.fishgold.co.il/isl/videos/" ++ id ++ ".mp4"
            , controls True
            , autoplay True
            , Html.Attributes.attribute "muted" "true"
            , Html.Attributes.attribute "playsinline" "true"
            , preload "auto"
            ]
            []
        ]
