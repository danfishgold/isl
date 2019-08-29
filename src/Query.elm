module Query exposing (Query, empty, fromList, isEmpty, removeLastBlock)


type alias Query block =
    { blocksBefore : List block
    , text : String
    }


empty : Query block
empty =
    { blocksBefore = []
    , text = ""
    }


fromList : List block -> Query block
fromList blocks =
    { blocksBefore = blocks
    , text = ""
    }


isEmpty : Query block -> Bool
isEmpty query =
    List.isEmpty query.blocksBefore && String.isEmpty query.text


removeLastBlock : Query block -> Query block
removeLastBlock query =
    { query | blocksBefore = List.take (List.length query.blocksBefore - 1) query.blocksBefore }
