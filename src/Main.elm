module Main exposing (..)

import Html exposing (Html, program, div, input, p, h2, text, video, source)
import Html.Keyed
import Html.Attributes exposing (dir, src, type_, width)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData, RemoteData(..))
import RemoteData.Http
import Dict exposing (Dict)
import Json.Decode as D
import Fuzzy


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { words : WebData (Dict String Word)
    , query : String
    , selectedWord : Maybe String
    }


type alias Word =
    { text : String
    , sources : Dict String String
    }


wordDictDecoder : D.Decoder (Dict String Word)
wordDictDecoder =
    D.map2 Word
        (D.field "text" D.string)
        (D.field "sources" (D.dict D.string))
        |> D.dict


init : ( Model, Cmd Msg )
init =
    ( { words = NotAsked
      , query = ""
      , selectedWord = Nothing
      }
    , RemoteData.Http.get "http://localhost:8000/combined.json" Words wordDictDecoder
    )


type Msg
    = Words (WebData (Dict String Word))
    | SetQuery String
    | ShowWord String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Words words ->
            ( { model | words = words }, Cmd.none )

        SetQuery q ->
            ( { model | query = q }, Cmd.none )

        ShowWord id ->
            ( { model
                | selectedWord = Just id
                , query = ""
              }
            , Cmd.none
            )


wordWithId : String -> WebData (Dict String String) -> Maybe String
wordWithId id data =
    data |> RemoteData.toMaybe |> Maybe.andThen (Dict.get id)


view : Model -> Html Msg
view model =
    case model.words of
        NotAsked ->
            text "Loading..."

        Loading ->
            text "Loading..."

        Success wordDict ->
            div [ dir "rtl" ]
                [ input [ onInput SetQuery ] []
                , wordDict
                    |> Dict.toList
                    |> Fuzzy.filterItems 1 model.query (Tuple.second >> .text)
                    |> List.map (\( id, word ) -> p [ onClick (ShowWord id) ] [ text word.text ])
                    |> div []
                , case model.selectedWord |> Maybe.andThen (\k -> Dict.get k wordDict) of
                    Nothing ->
                        text ""

                    Just word ->
                        Html.Keyed.node "div" [] [ ( "video-" ++ word.text, video word ) ]
                ]

        Failure error ->
            div []
                [ text "Error: "
                , text <| toString error
                ]


video : Word -> Html msg
video word =
    div []
        [ Html.h2 [] [ text word.text ]
        , word.sources
            |> Dict.toList
            |> List.map (\( mediaType, url ) -> source [ src url, type_ mediaType ] [])
            |> Html.video [ Html.Attributes.width 400, Html.Attributes.controls True ]
        ]
