module Main exposing (main)

import Array
import BlockBar
import Browser
import Browser.Navigation as Nav
import Colors
import Dictionary exposing (Dictionary, WordId)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Key exposing (Key)
import PlaybackRate
import Query exposing (Query)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Suggestions exposing (suggestions)
import Url exposing (Url)
import Util exposing (dir, segmentedControl, style)
import Video



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
    | RemoveBlockAtIndex Int



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

        RemoveBlockAtIndex idx ->
            { model | query = Query.removeBlockAtIndex idx model.query } |> andPushUrl


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
        [ Element.layout
            [ dir "rtl"
            , Font.family [ Font.typeface "arial", Font.sansSerif ]
            , padding 20
            ]
            (body model)
        ]
    }


body : Model -> Element Msg
body model =
    case model.dictionary of
        NotAsked ->
            Element.none

        Loading ->
            Element.none

        Failure err ->
            let
                _ =
                    Debug.log "error" err
            in
            text "uh oh"

        Success dict ->
            let
                normalWidth =
                    width (fill |> maximum 600)
            in
            column
                [ width fill
                , height fill
                , spacing 50
                ]
                [ column
                    [ width fill
                    , height fill
                    , spacing 20
                    ]
                    [ title
                    , column [ normalWidth, centerX, spacing 10 ]
                        [ text "מה בא לך להגיד בשפת הסימנים?"
                        , BlockBar.element InputKeyHit
                            SetQueryText
                            RemoveBlockAtIndex
                            (Dictionary.title dict)
                            model.query
                            (placeholder model.query)
                            [ width fill
                            , below <|
                                suggestions SelectSuggestion
                                    SetSuggestionIndex
                                    model.selectedSuggestion
                                    model.query
                                    [ width fill
                                    , height (shrink |> maximum 200)
                                    , style "overflow" "scroll"
                                    , Border.roundEach
                                        { topLeft = 0
                                        , topRight = 0
                                        , bottomLeft = 5
                                        , bottomRight = 5
                                        }
                                    , Border.solid
                                    , Border.color Colors.suggestions.border
                                    , Border.width 1
                                    ]
                            ]
                        , if Query.hasBlocks model.query then
                            el [ alignRight ] (PlaybackRate.control SetPlaybackRate model.playbackRate)

                          else
                            Element.none
                        ]
                    , if Query.isEmpty model.query then
                        el [ normalWidth, centerX, paddingXY 0 50 ] examples

                      else
                        videos dict (Query.blockList model.query)
                    ]
                , el [ normalWidth, centerX ] description
                ]


placeholder : Query block -> Maybe String
placeholder query =
    case List.length (Query.blockList query) of
        0 ->
            Just "חיפוש"

        1 ->
            Just "הוסיפו עוד מילים כדי להרכיב משפט"

        2 ->
            Just "הוסיפו עוד מילים כדי להרכיב משפט"

        _ ->
            Nothing


title : Element msg
title =
    link
        [ Background.color Colors.title.fill
        , Font.color Colors.title.text
        , Border.rounded 5
        , Font.size 48
        , Font.bold
        , padding 10
        , centerX
        ]
        { url = "#", label = text "מילון שפת הסימנים" }


videos : Dictionary -> List WordId -> Element Msg
videos dictionary words =
    words
        |> List.indexedMap (\idx word -> videoWrapper dictionary idx word)
        |> wrappedRow [ height fill, width shrink, centerX, spacing 20 ]


videoWrapper : Dictionary -> Int -> WordId -> Element Msg
videoWrapper dict wordIndex word =
    let
        title_ =
            Dictionary.title dict word
    in
    column [ alignTop ]
        [ row [ spacingXY 10 0, height (shrink |> minimum 50), dir "ltr" ]
            [ variationsControl dict wordIndex word
            , el [ dir "rtl" ] (text title_)
            ]
        , Video.element word
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
        segmentedControl Colors.variations (SetWordAtIndex wordIndex) word indexedOptions


examples : Element Msg
examples =
    column [ spacing 12 ]
        [ el
            [ paddingXY 7 5
            , Background.color Colors.examples.titleFill
            , Font.color Colors.examples.titleText
            , Border.rounded 5
            , Font.bold
            ]
            (text "דוגמאות ממש טובות")
        , [ ( "שלום! ברוכים הבאים למילון שפת הסימנים", "#LTKFKykj8Rc" )
          , ( "איך קוראים לך?", "#JSgVC0Ed" )
          , ( "איך אני מגיע לספריה?", "#0Qq9EKEvZR0" )
          ]
            |> List.map
                (\( phrase, url ) ->
                    link
                        [ Font.underline
                        , mouseOver [ Font.color Colors.examples.hoverText ]
                        ]
                        { url = url, label = text phrase }
                )
            |> column [ paddingXY 7 0, spacing 7 ]
        ]


description : Element msg
description =
    paragraph []
        [ text "מבוסס על "
        , link [ Font.underline ] { url = "http://isl.org.il/he/דף-הבית/", label = text "המילון" }
        , text " של "
        , link [ Font.underline ] { url = "https://www.sela.org.il", label = text "המכון לקידום החרש" }
        , text ". רוצים ללמוד לדבר בשפת הסימנים? אני ממליץ בחום על "
        , link [ Font.underline ] { url = "https://www.sela.org.il/קורס-שפת-סימנים/", label = text "הקורסים שלהם" }
        , text "."
        ]
