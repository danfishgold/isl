module Main exposing (main)

import About
import Array
import BlockBar
import Browser
import Browser.Dom as Dom
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
import Task
import Url exposing (Url)
import Util exposing (delayTask, id, segmentedControl, style)
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
    , inAbout : Bool
    }



-- MSG


type Msg
    = SetDictionary Locale (WebData Dictionary)
    | SetPlaybackRate Float
    | SetQueryText String
    | InputKeyHit Key
    | SetSuggestionIndex Int
    | SelectSuggestion WordId
    | SetWordAtIndex Int WordId
    | RemoveWordAtIndex Int
    | FocusError Dom.Error
    | NoOp
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest



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
        , inAbout = False
        }



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        SetDictionary locale dictionary ->
            ( { model | dictionary = L10n.set locale dictionary model.dictionary }
            , Cmd.none
            )

        SetPlaybackRate rate ->
            ( { model | playbackRate = rate }, PlaybackRate.set rate )

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
                        updateQuery (Query.removeLastBlock model.query) ( model, Cmd.none )

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

        SetSuggestionIndex idx ->
            ( { model | selectedSuggestion = Just idx }, Cmd.none )

        SelectSuggestion word ->
            addWordToQueryAndReset word model

        SetWordAtIndex idx word ->
            updateQuery (Query.setBlockAtIndex idx word model.query) ( model, Cmd.none )

        RemoveWordAtIndex idx ->
            updateQuery (Query.removeBlockAtIndex idx model.query) ( model, Cmd.none )

        FocusError error ->
            let
                _ =
                    Debug.log "focus error" error
            in
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        UrlChanged url ->
            updateModelWithUrl url model

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


addWordToQueryAndReset : WordId -> Model -> ( Model, Cmd Msg )
addWordToQueryAndReset word model =
    let
        newQuery =
            model.query
                |> Query.appendBlock word
                |> Query.clearText
    in
    updateQuery newQuery ( model, Cmd.none )


updateQuery : Query WordId -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateQuery query ( model, cmd ) =
    ( { model | query = query }
    , Cmd.batch
        [ cmd
        , Route.push model.key
            (Route.VideoList model.locale (Query.blockList query))
        , Dom.focus "search-bar"
            |> delayTask 30
            |> Task.attempt
                (\res ->
                    case res of
                        Err error ->
                            FocusError error

                        Ok () ->
                            NoOp
                )
        ]
    )


updateModelWithUrl : Url -> Model -> ( Model, Cmd Msg )
updateModelWithUrl url model =
    case Route.parse url of
        Just (Route.VideoList locale ids) ->
            let
                newModel =
                    { model
                        | locale = locale
                        , query = Query.fromList ids
                        , inAbout = False
                    }
            in
            ( newModel
            , if RemoteData.toMaybe (getDictionary newModel) == Nothing then
                Dictionary.fetch newModel.locale (SetDictionary newModel.locale)

              else
                Cmd.none
            )

        Just Route.About ->
            ( { model | inAbout = True, locale = Hebrew }, Cmd.none )

        Nothing ->
            ( model, Route.push model.key Route.default )


getDictionary : Model -> WebData Dictionary
getDictionary model =
    L10n.get model.locale model.dictionary



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = L10n.string model.locale .title
    , body =
        [ Element.layout
            [ L10n.dir model.locale
            , Font.family [ Font.typeface "arial", Font.sansSerif ]
            , padding 20
            ]
            (body model)
        ]
    }


normalWidth : Attribute msg
normalWidth =
    width (fill |> maximum 600)


mainBody : Model -> List (Element Msg)
mainBody model =
    if model.inAbout then
        About.view

    else
        case getDictionary model of
            NotAsked ->
                [ el [ normalWidth, centerX ] <| text (L10n.string model.locale .loading) ]

            Loading ->
                [ el [ normalWidth, centerX ] <| text (L10n.string model.locale .loading) ]

            Failure err ->
                let
                    _ =
                        Debug.log "error" err
                in
                [ el [ normalWidth, centerX ] <| text (L10n.string model.locale .errorMessage) ]

            Success dict ->
                [ column [ normalWidth, centerX, spacing 10 ]
                    [ text (L10n.string model.locale (.search >> .prompt))
                    , BlockBar.element InputKeyHit
                        SetQueryText
                        RemoveWordAtIndex
                        (Dictionary.title model.locale dict)
                        model.query
                        (L10n.string model.locale
                            (.search >> .placeholder)
                            (List.length (Query.blockList model.query))
                        )
                        [ width fill
                        , id "search-bar"
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


body : Model -> Element Msg
body model =
    column
        [ width fill
        , height fill
        , spacing 50
        ]
        [ navBar model
        , column
            [ width fill
            , height fill
            , spacing 30
            ]
            [ title model.locale
            , column [ width fill, height fill, spacing 20, alignTop ] (mainBody model)
            , el [ normalWidth, centerX ] (L10n.get model.locale footnote)
            ]
        ]


title : Locale -> Element msg
title locale =
    Element.link
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
                    Element.link
                        [ Font.underline
                        , mouseOver [ Font.color Colors.examples.hoverText ]
                        ]
                        { url = url, label = paragraph [] [ text phrase ] }
                )
            |> column [ spacing 7 ]
        ]


footnote : Localized (Element msg)
footnote =
    { hebrew =
        paragraph []
            [ text "מבוסס על המילון של המכון לקידום החרש (שלא קיים יותר). רוצים ללמוד לדבר בשפת הסימנים? הנה "
            , Util.link { url = "https://shasiclass.com", label = "אתר שמרכז קורסים ומורים מוסמכים" }
            , text "!"
            ]
    , english =
        paragraph []
            [ text "Based on SELA's dictionary (which doesn't exist anymore). If you want to learn ISL (in Isral) here's "
            , Util.link { url = "https://shasiclass.com", label = "a site with a list of courses and tutors" }
            , text "."
            ]
    }


navBar : Model -> Element msg
navBar model =
    row [ spacing 10 ] <|
        case model.locale of
            Hebrew ->
                [ Util.link
                    { label = "English"
                    , url =
                        Route.toString (Route.VideoList English (Query.blockList model.query))
                    }
                , el [ paddingXY 10 0 ]
                    (Util.link
                        { label = "אודות"
                        , url =
                            Route.toString Route.About
                        }
                    )
                ]

            English ->
                [ Util.link
                    { label = "עברית"
                    , url = Route.toString (Route.VideoList Hebrew (Query.blockList model.query))
                    }
                ]
