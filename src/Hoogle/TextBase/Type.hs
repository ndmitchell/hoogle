
module Hoogle.TextBase.Type where

import Hoogle.TypeSig.All


type TextBase = [TextItem]

data TextItem = ItemModule [String]
              | ItemClass TypeSig
              | ItemFunc String TypeSig
              | ItemAlias TypeSig TypeSig
              | ItemData DataKeyword TypeSig
              | ItemInstance TypeSig
              | ItemAttribute String String
              deriving (Show,Eq)


data DataKeyword = NewTypeKeyword
                 | DataKeyword
                 deriving (Show,Eq)


{- TODO: put this somewhere sensible
-- So that results are sorted in some rough order
-- lower is more powerful
itemPriority :: ItemRest -> Int
itemPriority x = case x of
    ItemKeyword{} -> 0
    ItemModule{} -> 1
    ItemClass{} -> 2
    ItemAlias{} -> 3
    ItemData{} -> 4
    ItemFunc{} -> 5
    _ -> 6
-}
