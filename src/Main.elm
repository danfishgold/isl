module Main exposing (..)

import Html exposing (Html, program, div, input, p, text)
import Html.Attributes exposing (dir)
import Html.Events exposing (onInput)
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
    { words : WebData (Dict String String)
    , query : String
    }


init : ( Model, Cmd Msg )
init =
    ( { words = NotAsked
      , query = ""
      }
    , RemoteData.Http.get "http://localhost:8000/words.json"
        Words
        (D.dict D.string)
    )


type Msg
    = Words (WebData (Dict String String))
    | SetQuery String


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
                    |> Fuzzy.filterItems 1 model.query Tuple.second
                    |> List.map (\( id, word ) -> p [] [ text word ])
                    |> div []
                ]

        Failure error ->
            div []
                [ text "Error"
                , text <| toString error
                ]
