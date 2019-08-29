module Query exposing (Query, addBlockAndResetText, blockList, empty, fromList, hasBlocks, isEmpty, removeBlockAtIndex, removeLastBlock)

import Array exposing (Array)
import Key exposing (Key)


type alias Query block =
    { blocksBefore : Array block
    , text : String
    }


empty : Query block
empty =
    { blocksBefore = Array.empty
    , text = ""
    }


fromList : List block -> Query block
fromList blocks =
    { blocksBefore = Array.fromList blocks
    , text = ""
    }


isEmpty : Query block -> Bool
isEmpty query =
    Array.isEmpty query.blocksBefore && String.isEmpty query.text


hasBlocks : Query block -> Bool
hasBlocks query =
    not <| Array.isEmpty query.blocksBefore


removeLastBlock : Query block -> Query block
removeLastBlock query =
    { query | blocksBefore = Array.slice 0 -1 query.blocksBefore }


removeBlockAtIndex : Int -> Query block -> Query block
removeBlockAtIndex idx query =
    { query
        | blocksBefore =
            Array.append
                (Array.slice 0 idx query.blocksBefore)
                (Array.slice (idx + 1) (Array.length query.blocksBefore) query.blocksBefore)
    }


addBlockAndResetText : block -> Query block -> Query block
addBlockAndResetText block q =
    { text = "", blocksBefore = Array.push block q.blocksBefore }


blockList : Query block -> List block
blockList query =
    Array.toList query.blocksBefore
