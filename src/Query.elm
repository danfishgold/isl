module Query exposing
    ( Query
    , appendBlock
    , blockList
    , blocksChanged
    , clearText
    , empty
    , fromList
    , hasBlocks
    , isEmpty
    , isTextEmpty
    , removeBlockAtIndex
    , removeLastBlock
    , setBlockAtIndex
    , setText
    , suggestions
    , text
    )

import Array exposing (Array)
import Fuzzy


type Query block
    = Query
        { blocksBefore : Array block
        , text : String
        , suggestions : Maybe (Array ( block, Fuzzy.Match ))
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
        , suggestions = Nothing
        }



-- PROPERTIES


isEmpty : Query block -> Bool
isEmpty query =
    not (hasBlocks query) && isTextEmpty query


hasBlocks : Query block -> Bool
hasBlocks (Query q) =
    not <| Array.isEmpty q.blocksBefore


isTextEmpty : Query block -> Bool
isTextEmpty (Query q) =
    String.isEmpty q.text


blocksChanged : Query block -> Query block -> Bool
blocksChanged (Query q1) (Query q2) =
    q1.blocksBefore /= q2.blocksBefore



-- ACCESS


blockList : Query block -> List block
blockList (Query q) =
    Array.toList q.blocksBefore


text : Query block -> String
text (Query q) =
    q.text


suggestions : Query block -> Maybe (Array ( block, Fuzzy.Match ))
suggestions (Query q) =
    q.suggestions



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


setBlockAtIndex : Int -> block -> Query block -> Query block
setBlockAtIndex idx block (Query q) =
    Query { q | blocksBefore = Array.set idx block q.blocksBefore }


appendBlock : block -> Query block -> Query block
appendBlock block (Query q) =
    Query { q | blocksBefore = Array.push block q.blocksBefore }



-- TEXT MANIPULATION


setText : List block -> (block -> String) -> String -> Query block -> Query block
setText possibleMatches blockToString newText (Query q) =
    Query
        { q
            | text = newText
            , suggestions =
                if String.isEmpty newText then
                    Nothing

                else
                    Fuzzy.filter newText blockToString possibleMatches
                        |> Array.fromList
                        |> Just
        }


clearText : Query block -> Query block
clearText (Query q) =
    Query { q | text = "", suggestions = Nothing }
