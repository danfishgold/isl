module Util exposing (maybeList)


maybeList : List (Maybe a) -> Maybe (List a)
maybeList xs =
    case xs of
        (Just hd) :: tl ->
            maybeList tl |> Maybe.map (\justTl -> hd :: justTl)

        Nothing :: _ ->
            Nothing

        [] ->
            Just []
