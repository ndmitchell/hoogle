
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
                 deriving Eq

instance Show DataKeyword where
    show NewTypeKeyword = "newtype"
    show DataKeyword = "data"
