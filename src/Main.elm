module Main exposing (main)

import Array
import BlockBar exposing (blockBar)
import Browser
import Browser.Navigation as Nav
import Dictionary exposing (Dictionary, WordId)
import Element exposing (..)
import Element.Font as Font
import Element.Lazy exposing (lazy)
import Html
import Html.Attributes exposing (autoplay, controls, dir, preload, src)
import Key exposing (Key)
import PlaybackRate
import Query exposing (Query)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Suggestions exposing (suggestions)
import Url exposing (Url)
import Util exposing (segmentedControl)



-- http://0.0.0.0:5500/#12837,4977,7509,7105,2773,3781,2837
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
    | SetWordAtIndex Int WordId
    | SetSuggestionIndex Int



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
    case msg of
        UrlChanged url ->
            updateModelWithUrl url model

        SetDictionary dictionary ->
            ( { model | dictionary = dictionary }, Cmd.none )

        SetQueryText text ->
            let
                newQuery =
                    case RemoteData.toMaybe model.dictionary of
                        Nothing ->
                            Query.setText [] (always "NO WORDS") text model.query

                        Just dict ->
                            Query.setText
                                (Dictionary.primaryWordList dict)
                                (Dictionary.title dict)
                                text
                                model.query
            in
            ( { model | query = newQuery, selectedSuggestion = Nothing }, Cmd.none )

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
                            Maybe.map2 (\suggestions idx -> Array.get idx suggestions)
                                (Query.suggestions model.query)
                                model.selectedSuggestion
                                |> Maybe.andThen identity
                                |> Maybe.map Tuple.first
                    in
                    case selectedWord of
                        Nothing ->
                            ( model, Cmd.none )

                        Just word ->
                            addWordToQueryAndReset word model

                Key.Backspace ->
                    if Query.isTextEmpty model.query then
                        { model | query = Query.removeLastBlock model.query }
                            |> andPushUrl

                    else
                        ( model, Cmd.none )

                Key.Up ->
                    let
                        suggestions =
                            Query.suggestions model.query |> Maybe.withDefault Array.empty

                        index =
                            model.selectedSuggestion |> Maybe.withDefault 0

                        newIndex =
                            if Array.isEmpty suggestions then
                                Nothing

                            else
                                Just (modBy (Array.length suggestions) (index - 1))
                    in
                    ( { model | selectedSuggestion = newIndex }, Cmd.none )

                Key.Down ->
                    let
                        suggestions =
                            Query.suggestions model.query |> Maybe.withDefault Array.empty

                        index =
                            model.selectedSuggestion |> Maybe.withDefault (Array.length suggestions - 1)

                        newIndex =
                            if Array.isEmpty suggestions then
                                Nothing

                            else
                                Just (modBy (Array.length suggestions) (index + 1))
                    in
                    ( { model | selectedSuggestion = newIndex }, Cmd.none )

        SelectSuggestion word ->
            addWordToQueryAndReset word model

        SetWordAtIndex idx word ->
            { model | query = Query.setBlockAtIndex idx word model.query } |> andPushUrl

        SetSuggestionIndex idx ->
            ( { model | selectedSuggestion = Just idx }, Cmd.none )


addWordToQueryAndReset : WordId -> Model -> ( Model, Cmd Msg )
addWordToQueryAndReset word model =
    let
        newQuery =
            model.query
                |> Query.appendBlock word
                |> Query.clearText
    in
    { model | query = newQuery } |> andPushUrl


andPushUrl : Model -> ( Model, Cmd Msg )
andPushUrl model =
    ( model, Route.push model.key (Route.VideoList (Query.blockList model.query)) )


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
                    [ Element.htmlAttribute (Html.Attributes.dir "ltr")
                    , Font.family [ Font.typeface "arial", Font.sansSerif ]
                    ]
                  <|
                    Element.column [ height fill ]
                        [ blockBar InputKeyHit SetQueryText (Dictionary.title dict) model.query
                        , el
                            []
                            (suggestions SelectSuggestion SetSuggestionIndex model.selectedSuggestion model.query)
                        , if Query.hasBlocks model.query then
                            PlaybackRate.control SetPlaybackRate model.playbackRate

                          else
                            Element.none
                        , videos dict (Query.blockList model.query)
                        , description
                        ]
                ]
    }


videos : Dictionary -> List WordId -> Element Msg
videos dictionary words =
    words
        |> List.indexedMap (\idx word -> videoWrapper dictionary idx word)
        |> wrappedRow [ height fill ]


videoWrapper : Dictionary -> Int -> WordId -> Element Msg
videoWrapper dict wordIndex word =
    let
        title =
            Dictionary.title dict word
    in
    column []
        [ row [ spacing 20 ]
            [ text title
            , variationsControl dict wordIndex word
            ]
        , video word
        ]


variationsControl : Dictionary -> Int -> WordId -> Element Msg
variationsControl dict wordIndex word =
    let
        group =
            Dictionary.group dict word
    in
    if List.isEmpty group.variations then
        Element.none

    else
        let
            indexedOptions =
                group.primary
                    :: group.variations
                    |> List.indexedMap (\idx opt -> ( opt, String.fromInt (idx + 1) ))
        in
        segmentedControl (SetWordAtIndex wordIndex) word indexedOptions


video : WordId -> Element msg
video wordId =
    Element.html <|
        Html.video
            [ src <| "http://files.fishgold.co.il/isl/videos/" ++ Dictionary.wordIdToString wordId ++ ".mp4"
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
        , link [ Font.underline ] { url = "http://isl.org.il/he/דף-הבית/", label = text "המילון" }
        , text " של "
        , link [ Font.underline ] { url = "https://www.sela.org.il", label = text "המכון לקידום החרש" }
        , text ". רוצים ללמוד שפת סימנים? אני ממליץ בחום על "
        , link [ Font.underline ] { url = "https://www.sela.org.il/קורס-שפת-סימנים/", label = text "הקורסים שלהם" }
        , text "."
        ]
