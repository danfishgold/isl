module Main exposing (main)

import BlockBar exposing (blockBar)
import Browser
import Browser.Navigation as Nav
import Dict
import Dictionary exposing (Dictionary)
import Element exposing (Element)
import Html exposing (Html, button, div, h2, text, video)
import Html.Attributes exposing (autoplay, controls, dir, preload, src, style)
import Html.Events exposing (onClick)
import Html.Keyed exposing (node)
import Key exposing (Key)
import PlaybackRate
import Query exposing (Query)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Suggestions exposing (suggestions)
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
    , query : Query Int
    , playbackRate : Float
    , key : Nav.Key
    }



-- MSG


type Msg
    = SetDictionary (WebData Dictionary)
    | SetQueryText String
    | SetPlaybackRate Float
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | InputKeyHit Key



-- INIT


type alias Flags =
    ()


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        ( model, pageCmd ) =
            updateModelWithUrl url
                { dictionary = Loading
                , query = Query.empty
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

        SetQueryText text ->
            let
                q =
                    model.query
            in
            ( { model | query = { q | text = text } }
            , Cmd.batch
                [ Cmd.none --Route.push model.key (Route.VideoList ids)
                , if model.query.blocksBefore /= q.blocksBefore then
                    PlaybackRate.setDelayed SetPlaybackRate model.playbackRate

                  else
                    Cmd.none
                ]
            )

        SetPlaybackRate rate ->
            ( { model | playbackRate = rate }, PlaybackRate.set rate )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        InputKeyHit key ->
            case key of
                Key.Enter ->
                    ( model, Cmd.none )

                Key.Backspace ->
                    ( model, Cmd.none )

                Key.Up ->
                    ( model, Cmd.none )

                Key.Down ->
                    ( model, Cmd.none )


updateModelWithUrl : Url -> Model -> ( Model, Cmd Msg )
updateModelWithUrl url model =
    case Route.parse url of
        Route.Home ->
            ( { model | query = Query.empty }
            , Cmd.none
            )

        Route.VideoList ids ->
            ( { model | query = Query.fromList ids }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "מילון שפת הסימנים"
    , body =
        case model.dictionary of
            NotAsked ->
                []

            Loading ->
                []

            Failure _ ->
                [ text "fail" ]

            Success dict ->
                [ Element.layout [ Element.htmlAttribute (Html.Attributes.dir "rtl") ] <|
                    Element.column []
                        [ blockBar InputKeyHit SetQueryText (always "heyyy") model.query
                        , suggestions dict model.query.text
                        , if List.length model.query.blocksBefore > 0 then
                            PlaybackRate.control SetPlaybackRate model.playbackRate

                          else
                            Element.none
                        , model.query.blocksBefore
                            |> List.filterMap (\k -> Dict.get k dict.words |> Maybe.map (\w -> ( k, w )))
                            |> List.indexedMap (\idx ( id, word ) -> ( id, video id ))
                            -- |> node "div" [ style "display" "flex", style "flex-wrap" "wrap" ]
                            |> always (Element.text "video")
                        ]
                ]
    }



-- body : Model -> Html Msg
-- body model =
--     case model.dictionary of
--         NotAsked ->
--             text "Loading..."
--         Loading ->
--             text "Loading..."
--         Success dict ->
--             div [ dir "rtl" ]
--                 [ SearchBar.view
--                     SetQueryText
--                     ShowWords
--                     (\newIds -> ShowWords (model.selectedWords ++ newIds))
--                     dict
--                     model.query
-- , if List.length model.selectedWords > 0 then
--     PlaybackRate.control SetPlaybackRate model.playbackRate
--   else
--     text ""
-- , model.selectedWords
--     |> List.filterMap (\k -> Dict.get k dict.words |> Maybe.map (\w -> ( k, w )))
--     |> List.indexedMap (\idx ( id, word ) -> ( id, video id word (RemoveWord idx) ))
--     |> node "div" [ style "display" "flex", style "flex-wrap" "wrap" ]
-- ]
--         Failure _ ->
--             div []
--                 [ text "אוי לא! היתה שגיאת רשת"
--                 ]


video : Int -> Element msg
video id =
    Element.html <|
        Html.video
            [ src <| "http://files.fishgold.co.il/isl/videos/" ++ String.fromInt id ++ ".mp4"
            , controls True
            , autoplay True
            , Html.Attributes.attribute "muted" "true"
            , Html.Attributes.attribute "playsinline" "true"
            , preload "auto"
            ]
            []
