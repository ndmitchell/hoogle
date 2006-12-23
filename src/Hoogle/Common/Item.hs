
module Hoogle.Common.Item where

import Hoogle.TypeSig.All


-- define what an "Item" is, since it crops up so much
-- allow enough flexibility for all users

data Item a = Item {
                  itemMod :: Maybe Module,
                  itemName :: Maybe String,
                  itemType :: Maybe TypeVal,
                  itemId :: Maybe Int,
                  itemExtra :: a,
                  itemRest :: ItemRest
              }
              deriving Show
            

blankItem = Item Nothing Nothing Nothing Nothing () ItemUnknown
            

data Module = Module {modName :: [String], modId :: Int}
              deriving Show

data ItemRest = ItemModule
              | ItemClass LHS
              | ItemFunc
              | ItemAlias LHS TypeVal
              | ItemData DataKeyword LHS
              | ItemInstance TypeSig
              | ItemKeyword
              | ItemAttribute String String
              | ItemUnknown
              deriving Show


isItemAttribute (ItemAttribute{}) = True
isItemAttribute _ = False

isItemInstance (ItemInstance{}) = True
isItemInstance _ = False

isItemModule (ItemModule) = True
isItemModule _ = False

isItemFunc (ItemFunc) = True
isItemFunc _ = False


-- name is not given here
data LHS = LHS Constraint [String] -- context => name vars
         | LHSStr String String
         deriving Show


data TypeVal = TypeAST TypeSig
             | TypeStr String
             | TypeArgs String [String] -- context => arg0 -> arg1 .. -> argn
             deriving Show


data DataKeyword = NewTypeKeyword
                 | DataKeyword
                 deriving Enum

instance Show DataKeyword where
    show NewTypeKeyword = "newtype"
    show DataKeyword = "data"


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
