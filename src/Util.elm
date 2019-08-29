module Util exposing (bytesDecodeList, listAt, maybeList, segmentedControl)

import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


segmentedControl : (option -> msg) -> option -> List ( option, String ) -> Element msg
segmentedControl onSelect selected options =
    let
        optionButton ( option, label ) =
            if option == selected then
                Input.button
                    [ paddingXY 10 0
                    , Background.color (rgb 1 0 0)
                    , Font.color (rgb 1 1 1)
                    , Border.rounded 5
                    ]
                    { label = text label
                    , onPress =
                        Nothing
                    }

            else
                Input.button
                    [ paddingXY 10 0
                    , Border.rounded 5
                    , mouseOver
                        [ Background.color (rgb 1 0.6 0.6)
                        , Font.color (rgb 0 0 0)
                        ]
                    ]
                    { label = text label
                    , onPress =
                        Just (onSelect option)
                    }
    in
    List.map optionButton options
        |> row
            [ spacing 5
            , padding 5
            , Border.rounded 5
            , Background.color (rgb 0.9 0.9 0.9)
            ]


maybeList : List (Maybe a) -> Maybe (List a)
maybeList xs =
    case xs of
        (Just hd) :: tl ->
            maybeList tl |> Maybe.map (\justTl -> hd :: justTl)

        Nothing :: _ ->
            Nothing

        [] ->
            Just []


listAt : Int -> List a -> Maybe a
listAt idx xs =
    xs |> List.drop idx |> List.head


bytesDecodeList : Int -> BD.Decoder a -> Bytes -> Maybe (List a)
bytesDecodeList count decoder bytes =
    let
        helper ( xs, n ) =
            if n > 0 then
                decoder |> BD.map (\x -> BD.Loop ( x :: xs, n - 1 ))

            else
                BD.succeed (BD.Done (List.reverse xs))
    in
    BD.decode (BD.loop ( [], count ) helper) bytes
