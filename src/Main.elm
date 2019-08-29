module Main exposing (main)

import BlockBar exposing (blockBar)
import Browser
import Browser.Navigation as Nav
import Dictionary exposing (Dictionary, WordId)
import Element exposing (..)
import Element.Font as Font
import Element.Lazy exposing (lazy2)
import Fuzzy
import Html
import Html.Attributes exposing (autoplay, controls, dir, preload, src)
import Key exposing (Key)
import PlaybackRate
import Query exposing (Query)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Suggestions exposing (suggestions)
import Url exposing (Url)
import Util



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
    , query : Query WordId
    , selectedSuggestion : Maybe Int
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
    | SelectSuggestion WordId



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
                , selectedSuggestion = Nothing
                , playbackRate = 1
                , key = key
                }
    in
    ( model, Cmd.batch [ pageCmd, Dictionary.fetch SetDictionary ] )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
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
                    let
                        selectedWord =
                            Maybe.map2 Util.listAt
                                model.selectedSuggestion
                                (RemoteData.toMaybe model.dictionary
                                    |> Maybe.map Dictionary.groupList
                                    |> Maybe.map (Fuzzy.filter model.query.text Tuple.first)
                                )
                                |> Maybe.andThen
                                    (identity
                                        >> Maybe.map
                                            (Tuple.first
                                                >> Tuple.second
                                                >> .primary
                                            )
                                    )
                    in
                    case selectedWord of
                        Nothing ->
                            ( model, Cmd.none )

                        Just word ->
                            addWordToQueryAndReset word model

                Key.Backspace ->
                    if String.isEmpty model.query.text then
                        ( { model | query = Query.removeLastBlock model.query }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Key.Up ->
                    ( model, Cmd.none )

                Key.Down ->
                    ( model, Cmd.none )

        SelectSuggestion word ->
            addWordToQueryAndReset word model


addWordToQueryAndReset : WordId -> Model -> ( Model, Cmd Msg )
addWordToQueryAndReset word model =
    let
        newQuery =
            Query.addBlockAndResetText word model.query
    in
    ( { model | query = newQuery }
    , Route.push model.key (Route.VideoList (Query.blockList newQuery))
    )


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

            Failure err ->
                let
                    _ =
                        Debug.log "error" err
                in
                [ Html.text "uh oh" ]

            Success dict ->
                [ Element.layout
                    [ Element.htmlAttribute (Html.Attributes.dir "rtl")
                    , Font.family [ Font.typeface "arial", Font.sansSerif ]
                    ]
                  <|
                    Element.column [ height fill ]
                        [ blockBar InputKeyHit SetQueryText (Dictionary.title dict) model.query
                        , suggestions SelectSuggestion dict model.query.text
                        , if Query.hasBlocks model.query then
                            PlaybackRate.control SetPlaybackRate model.playbackRate

                          else
                            Element.none
                        , lazy2 videos dict (Query.blockList model.query)
                        , description
                        ]
                ]
    }


videos : Dictionary -> List WordId -> Element Msg
videos dictionary words =
    words
        |> List.map (videoWrapper dictionary)
        |> wrappedRow [ height fill ]


videoWrapper : Dictionary -> WordId -> Element Msg
videoWrapper dict id =
    column []
        [ text (Dictionary.title dict id)
        , video id
        ]


video : WordId -> Element msg
video id =
    Element.html <|
        Html.video
            [ src <| "http://files.fishgold.co.il/isl/videos/" ++ Dictionary.wordIdToString id ++ ".mp4"
            , controls True
            , autoplay True
            , Html.Attributes.attribute "muted" "true"
            , Html.Attributes.attribute "playsinline" "true"
            , preload "auto"
            ]
            []


description : Element msg
description =
    paragraph []
        [ text "מבוסס על "
        , link [ Font.underline ] { url = "http://isl.org.il/he/דף-הבית/", label = text "המילון הנהדר" }
        , text " של "
        , link [ Font.underline ] { url = "https://www.sela.org.il", label = text "המכון לקידום החרש" }
        , text ". רוצים ללמוד שפת סימנים? אני ממליץ בחום על "
        , link [ Font.underline ] { url = "https://www.sela.org.il/קורס-שפת-סימנים/", label = text "הקורסים שלהם" }
        , text "."
        ]
