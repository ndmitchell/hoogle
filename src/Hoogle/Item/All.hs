
module Hoogle.Item.All where

import Hoogle.TypeSig.All
import Data.Binary.Defer


-- | Blank items
blankItem :: Item
blankItem = Item 0 "" blankModule undefined

blankModule :: Module
blankModule = Module 0 []


type ItemId = Int
type ModId = Int


-- | An Item
data Item = Item {
                  itemId   :: ItemId, -- ^ an ID for the item
                  itemName :: String, -- ^ The name of the item (blank for instance or attribute)
                  itemMod  :: Module, -- ^ The module this item occurs within
                  itemRest :: ItemRest -- ^ Further information, specific to its type
              }
              deriving Show


-- | A module in which functions reside
data Module = Module {
                  modId :: ModId, -- ^ an ID for the module
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

instance BinaryDefer Item where
    bothDefer = defer [\ ~(Item a b c d) -> unit Item << a << b << c << d]

instance BinaryDefer Module where
    bothDefer = defer [\ ~(Module a b) -> unit Module << a << b]

instance BinaryDefer ItemRest where
    bothDefer = defer
        [\ ~(x@ItemModule) -> unit ItemModule <<! x
        ,\ ~(ItemClass a) -> unit ItemClass << a
        ,\ ~(ItemFunc a) -> unit ItemFunc << a
        ,\ ~(ItemAlias a b) -> unit ItemAlias << a << b
        ,\ ~(ItemData a b) -> unit ItemData << a << b
        ,\ ~(ItemInstance a) -> unit ItemInstance << a
        ,\ ~(x@ItemKeyword) -> unit ItemKeyword <<! x
        ,\ ~(ItemAttribute a b) -> unit ItemAttribute << a << b
        ]


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


instance BinaryDefer LHS where
    bothDefer = defer [\ ~(LHS a b) -> unit LHS << a << b
                      ,\ ~(LHSStr a b) -> unit LHSStr << a << b ]

instance BinaryDefer TypeVal where
    bothDefer = defer [\ ~(TypeAST a) -> unit TypeAST << a
                      ,\ ~(TypeStr a) -> unit TypeStr << a
                      ,\ ~(TypeArgs a b) -> unit TypeArgs << a << b ]

instance BinaryDefer DataKeyword where
    bothDefer = defer [\ ~(NewTypeKeyword) -> unit NewTypeKeyword
                      ,\ ~(DataKeyword) -> unit DataKeyword ]


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
