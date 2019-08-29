module Query exposing
    ( Query
    , appendBlock
    , blockList
    , blocksChanged
    , empty
    , fromList
    , hasBlocks
    , isEmpty
    , isTextEmpty
    , removeBlockAtIndex
    , removeLastBlock
    , setText
    , text
    )

import Array exposing (Array)


type Query block
    = Query
        { blocksBefore : Array block
        , text : String
        }



-- CREATION


empty : Query block
empty =
    fromList []


fromList : List block -> Query block
fromList blocks =
    Query
        { blocksBefore = Array.fromList blocks
        , text = ""
        }



-- EMPTINESS


isEmpty : Query block -> Bool
isEmpty query =
    not (hasBlocks query) && isTextEmpty query


hasBlocks : Query block -> Bool
hasBlocks (Query q) =
    not <| Array.isEmpty q.blocksBefore


isTextEmpty : Query block -> Bool
isTextEmpty (Query q) =
    String.isEmpty q.text



-- ACCESS


blockList : Query block -> List block
blockList (Query q) =
    Array.toList q.blocksBefore


text : Query block -> String
text (Query q) =
    q.text



-- BLOCK MANIPULATION


removeLastBlock : Query block -> Query block
removeLastBlock (Query q) =
    Query { q | blocksBefore = Array.slice 0 -1 q.blocksBefore }


removeBlockAtIndex : Int -> Query block -> Query block
removeBlockAtIndex idx (Query q) =
    Query
        { q
            | blocksBefore =
                Array.append
                    (Array.slice 0 idx q.blocksBefore)
                    (Array.slice (idx + 1) (Array.length q.blocksBefore) q.blocksBefore)
        }


appendBlock : block -> Query block -> Query block
appendBlock block (Query q) =
    Query { q | blocksBefore = Array.push block q.blocksBefore }



-- TEXT MANIPULATION


setText : String -> Query block -> Query block
setText newText (Query q) =
    Query { q | text = newText }



-- OTHER


blocksChanged : Query block -> Query block -> Bool
blocksChanged (Query q1) (Query q2) =
    q1.blocksBefore /= q2.blocksBefore
