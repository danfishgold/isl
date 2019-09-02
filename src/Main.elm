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
import Localization as L10n exposing (Locale(..), Localized)
import PlaybackRate
import Query exposing (Query)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Suggestions exposing (suggestions)
import Url exposing (Url)
import Util exposing (segmentedControl, style)
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
    { dictionary : Localized (WebData Dictionary)
    , query : Query WordId
    , selectedSuggestion : Maybe Int
    , playbackRate : Float
    , key : Nav.Key
    , locale : Locale
    }



-- MSG


type Msg
    = SetDictionary Locale (WebData Dictionary)
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
    updateModelWithUrl url
        { dictionary = { hebrew = Loading, english = NotAsked }
        , query = Query.empty
        , selectedSuggestion = Nothing
        , playbackRate = 1
        , key = key
        , locale = Hebrew
        }



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

        SetDictionary locale dictionary ->
            ( { model | dictionary = L10n.set locale dictionary model.dictionary }
            , Cmd.none
            )

        SetQueryText text ->
            let
                newQuery =
                    case getDictionary model |> RemoteData.toMaybe of
                        Nothing ->
                            Query.setText [] (always "NO WORDS") text model.query

                        Just dict ->
                            Query.setText
                                (Dictionary.primaryWordList dict)
                                (Dictionary.title model.locale dict)
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
    ( model
    , Route.push model.key
        (Route.VideoList model.locale (Query.blockList model.query))
    )


updateModelWithUrl : Url -> Model -> ( Model, Cmd Msg )
updateModelWithUrl url model =
    case Route.parse url of
        Just (Route.VideoList locale ids) ->
            let
                newModel =
                    { model | locale = locale, query = Query.fromList ids }
            in
            ( newModel
            , if RemoteData.toMaybe (getDictionary newModel) == Nothing then
                Dictionary.fetch newModel.locale (SetDictionary newModel.locale)

              else
                Cmd.none
            )

        Nothing ->
            ( model, Route.push model.key Route.default )


getDictionary : Model -> WebData Dictionary
getDictionary model =
    L10n.get model.locale model.dictionary



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "מילון שפת הסימנים"
    , body =
        [ Element.layout
            [ L10n.dir model.locale
            , Font.family [ Font.typeface "arial", Font.sansSerif ]
            , padding 20
            ]
            (body model)
        ]
    }


body : Model -> Element Msg
body model =
    case getDictionary model of
        NotAsked ->
            text <| L10n.localeToString model.locale ++ " " ++ "not asked"

        Loading ->
            text <| L10n.localeToString model.locale ++ " " ++ "loading"

        Failure err ->
            let
                _ =
                    Debug.log "error" err
            in
            text <| L10n.localeToString model.locale ++ " " ++ "uh oh"

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
                [ link [ Font.underline, mouseOver [ Font.color Colors.languageHover ] ]
                    (case model.locale of
                        Hebrew ->
                            { label = text "English"
                            , url =
                                Route.toString (Route.VideoList English (Query.blockList model.query))
                            }

                        English ->
                            { label = text "עברית"
                            , url = Route.toString (Route.VideoList Hebrew (Query.blockList model.query))
                            }
                    )
                , column
                    [ width fill
                    , height fill
                    , spacing 20
                    ]
                    [ title model.locale
                    , column [ normalWidth, centerX, spacing 10 ]
                        [ text (L10n.string model.locale (.search >> .prompt))
                        , BlockBar.element InputKeyHit
                            SetQueryText
                            RemoveBlockAtIndex
                            (Dictionary.title model.locale dict)
                            model.query
                            (L10n.string model.locale (.search >> .placeholder) <| List.length (Query.blockList model.query))
                            [ width fill
                            , below <|
                                suggestions model.locale
                                    SelectSuggestion
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
                            el [ alignRight ] (PlaybackRate.control model.locale SetPlaybackRate model.playbackRate)

                          else
                            Element.none
                        ]
                    , if Query.isEmpty model.query then
                        el [ normalWidth, centerX, paddingXY 0 30 ] (examples model.locale)

                      else
                        videos model.locale dict (Query.blockList model.query)
                    ]
                , el [ normalWidth, centerX ] (L10n.get model.locale description)
                ]


title : Locale -> Element msg
title locale =
    link
        [ Background.color Colors.title.fill
        , Font.color Colors.title.text
        , Border.rounded 5
        , Font.size 48
        , Font.bold
        , padding 10
        , centerX
        ]
        { url = Route.toString (Route.VideoList locale [])
        , label = paragraph [ Font.center ] [ text (L10n.string locale .title) ]
        }


videos : Locale -> Dictionary -> List WordId -> Element Msg
videos locale dictionary words =
    words
        |> List.indexedMap (\idx word -> videoWrapper locale dictionary idx word)
        |> wrappedRow [ height fill, width shrink, centerX, spacing 20 ]


videoWrapper : Locale -> Dictionary -> Int -> WordId -> Element Msg
videoWrapper locale dict wordIndex word =
    let
        title_ =
            Dictionary.title locale dict word
    in
    column [ alignTop ]
        [ L10n.container locale
            row
            [ spacingXY 10 0, height (shrink |> minimum 50) ]
            [ el [ L10n.dir locale ] (text title_)
            , variationsControl locale dict wordIndex word
            ]
        , Video.element word
        ]


variationsControl : Locale -> Dictionary -> Int -> WordId -> Element Msg
variationsControl locale dict wordIndex word =
    let
        group =
            Dictionary.group locale dict word
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


examples : Locale -> Element Msg
examples locale =
    column [ spacing 12 ]
        [ el
            [ paddingXY 7 5
            , if L10n.isLtr locale then
                moveLeft 7

              else
                moveRight 7
            , Background.color Colors.examples.titleFill
            , Font.color Colors.examples.titleText
            , Border.rounded 5
            , Font.bold
            ]
            (text <| L10n.string locale (.examples >> .title))
        , L10n.string locale (.examples >> .list)
            |> List.map
                (\( phrase, url ) ->
                    link
                        [ Font.underline
                        , mouseOver [ Font.color Colors.examples.hoverText ]
                        ]
                        { url = url, label = paragraph [] [ text phrase ] }
                )
            |> column [ spacing 7 ]
        ]


description : Localized (Element msg)
description =
    { hebrew =
        paragraph []
            [ text "מבוסס על "
            , link [ Font.underline ] { url = "http://isl.org.il/he/דף-הבית/", label = text "המילון" }
            , text " של "
            , link [ Font.underline ] { url = "https://www.sela.org.il", label = text "המכון לקידום החרש" }
            , text ". רוצים ללמוד לדבר בשפת הסימנים? אני ממליץ בחום על "
            , link [ Font.underline ] { url = "https://www.sela.org.il/קורס-שפת-סימנים/", label = text "הקורסים שלהם" }
            , text "."
            ]
    , english =
        paragraph []
            [ text "Based on "
            , link [ Font.underline ] { url = "https://www.sela.org.il", label = text "SELA" }
            , text "'s "
            , link [ Font.underline ] { url = "http://isl.org.il/he/דף-הבית/", label = text "dictionary" }
            , text ". If you want to learn ISL (and you live in Israel), I highly recommend their "
            , link [ Font.underline ] { url = "https://www.sela.org.il/קורס-שפת-סימנים/", label = text "courses" }
            , text "."
            ]
    }
