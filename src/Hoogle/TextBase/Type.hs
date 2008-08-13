
module Hoogle.TextBase.Type where

import Hoogle.TypeSig.All
import Data.Binary.Defer
import General.Code


-- (the item, any haddock documentation)
type TextBase = [(TextItem,String)]

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
                 deriving (Enum,Eq)

instance Show DataKeyword where
    show NewTypeKeyword = "newtype"
    show DataKeyword = "data"


instance BinaryDefer TextItem where
    put (ItemModule    a  ) = putByte 0 >> put1 a
    put (ItemClass     a  ) = putByte 1 >> put1 a
    put (ItemFunc      a b) = putByte 2 >> put2 a b
    put (ItemAlias     a b) = putByte 3 >> put2 a b
    put (ItemData      a b) = putByte 4 >> put2 a b
    put (ItemInstance  a  ) = putByte 5 >> put1 a
    put (ItemAttribute a b) = putByte 6 >> put2 a b

    get = do
        x <- getByte
        case x of
            0 -> get1 ItemModule
            1 -> get1 ItemClass
            2 -> get2 ItemFunc
            3 -> get2 ItemAlias
            4 -> get2 ItemData
            5 -> get1 ItemInstance
            6 -> get2 ItemAttribute

-- TODO: Add a putEnum/getEnum
instance BinaryDefer DataKeyword where
    put = putByte . fromEnum
    get = liftM toEnum getByte
