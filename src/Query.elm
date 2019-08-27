module Query exposing (Query, empty, isEmpty)


type alias Query =
    { blocksBefore : List String
    , text : String
    }


empty : Query
empty =
    { blocksBefore = []
    , text = ""
    }


isEmpty : Query -> Bool
isEmpty query =
    List.isEmpty query.blocksBefore && String.isEmpty query.text
