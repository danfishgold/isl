module Util exposing
    ( bytesDecodeList
    , delayTask
    , dictConcatMap
    , dictFilterMap
    , dir
    , id
    , link
    , listAt
    , maybeList
    , segmentedControl
    , style
    )

import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Colors
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Process
import Task exposing (Task)


style : String -> String -> Element.Attribute msg
style attr val =
    htmlAttribute (Html.Attributes.style attr val)


dir : String -> Element.Attribute msg
dir direction =
    htmlAttribute (Html.Attributes.dir direction)


id : String -> Element.Attribute msg
id id_ =
    htmlAttribute (Html.Attributes.id id_)


segmentedControl : Colors.SegmentedControl -> (option -> msg) -> option -> List ( option, String ) -> Element msg
segmentedControl colors onSelect selected options =
    let
        optionButton ( option, label ) =
            if option == selected then
                Input.button
                    [ paddingXY 10 5
                    , Background.color colors.selectedFill
                    , Font.color colors.selectedText
                    , Border.rounded 5
                    ]
                    { label = text label
                    , onPress =
                        Nothing
                    }

            else
                Input.button
                    [ paddingXY 10 5
                    , Border.rounded 5
                    , mouseOver
                        [ Background.color colors.hoverFill
                        , Font.color colors.hoverText
                        ]
                    ]
                    { label = text label
                    , onPress =
                        Just (onSelect option)
                    }
    in
    List.map optionButton options
        |> row
            [ spacing 3
            , paddingXY 3 5
            , Border.rounded 5
            , Background.color colors.backgroundFill
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


dictFilterMap : (comparable -> v1 -> Maybe v2) -> Dict comparable v1 -> Dict comparable v2
dictFilterMap map dict =
    Dict.foldl (\key val newDict -> Dict.update key (always (map key val)) newDict) Dict.empty dict


dictConcatMap : (comparable1 -> v -> Maybe comparable2) -> (v -> v -> v) -> Dict comparable1 v -> Dict comparable2 v
dictConcatMap map mergeValues dict =
    Dict.foldl
        (\key val newDict ->
            case map key val of
                Nothing ->
                    newDict

                Just newKey ->
                    Dict.update newKey (Maybe.map (mergeValues val) >> Maybe.withDefault val >> Just) newDict
        )
        Dict.empty
        dict


delayTask : Float -> Task x a -> Task x a
delayTask delay task =
    Process.sleep delay
        |> Task.andThen (always task)


link : { label : String, url : String } -> Element msg
link { label, url } =
    Element.link [ Font.underline, mouseOver [ Font.color Colors.linkHover ] ]
        { url = url
        , label = text label
        }
