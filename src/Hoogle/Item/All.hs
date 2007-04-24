
module Hoogle.Item.All where

import Hoogle.TypeSig.All


-- | Blank items
blankItem :: Item
blankItem = Item 0 "" blankModule undefined

blankModule :: Module
blankModule = Module 0 []


-- | An Item
data Item = Item {
                  itemId   :: Int, -- ^ an ID for the item
                  itemName :: String, -- ^ The name of the item (blank for instance or attribute)
                  itemMod  :: Module, -- ^ The module this item occurs within
                  itemRest :: ItemRest -- ^ Further information, specific to its type
              }
              deriving Show


-- | A module in which functions reside
data Module = Module {
                  modId :: Int, -- ^ an ID for the module
                  modName :: [String] -- ^ the name of the module
              }
              deriving Show


data ItemRest = ItemModule -- ^ Module A.B.C is mod=A.B name=C
              | ItemClass LHS
              | ItemFunc TypeVal
              | ItemAlias LHS TypeVal
              | ItemData DataKeyword LHS
              | ItemInstance TypeSig
              | ItemKeyword
              | ItemAttribute String String
              deriving Show


isItemAttribute (ItemAttribute{}) = True
isItemAttribute _ = False

isItemInstance (ItemInstance{}) = True
isItemInstance _ = False

isItemModule (ItemModule) = True
isItemModule _ = False

isItemFunc (ItemFunc{}) = True
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
