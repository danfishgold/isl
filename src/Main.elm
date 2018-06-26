module Main exposing (..)

import Html exposing (Html, program, text)
import RemoteData exposing (WebData, RemoteData(..))
import RemoteData.Http
import Dict exposing (Dict)


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { words : WebData (Dict Int String) }


init : ( Model, Cmd Msg )
init =
    ( { words = NotAsked }
    , Cmd.none
    )


type Msg
    = Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text "Hello"
