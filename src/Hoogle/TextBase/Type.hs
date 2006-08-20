
module Hoogle.TextBase.Type where

import Hoogle.TypeSig.Type


data DataKeyword = NewTypeKeyword
                 | DataKeyword
                 deriving Show

type TextBase = [Item]

data Item = Module [String]
          | Class TypeSig
          | Func String TypeSig
          | TypeAlias TypeSig TypeSig
          | Data DataKeyword TypeSig
          | Instance TypeSig
          | Keyword String
          deriving Show
