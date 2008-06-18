
module Hoogle.DataBase.Suggest where

import Data.Binary.Defer
import Data.Binary.Defer.Trie


newtype Suggest = Suggest (Trie [SuggestItem]) deriving Show

data SuggestItem = Ctor String -- constructor (and who the type is)
                 | Data String [Int] -- data type, name (case correct), and possible kinds
                 | Class String [Int] -- class, name (case correct), kinds
                   deriving Show


instance BinaryDefer SuggestItem where
    put (Ctor x) = putByte 0 >> put x
    put (Data x y) = putByte 1 >> put x >> put y
    put (Class x y) = putByte 2 >> put x >> put y

    get = do i <- getByte
             case i of
                0 -> get1 Ctor
                1 -> get2 Data
                2 -> get2 Class

instance BinaryDefer Suggest where
    put (Suggest x) = put x
    get = get1 Suggest


createSuggest :: a -> Suggest
createSuggest _ = Suggest $ newTrie []
