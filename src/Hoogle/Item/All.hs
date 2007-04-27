
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
              | ItemClass Lhs
              | ItemFunc TypeVal
              | ItemAlias Lhs TypeVal
              | ItemData DataKeyword Lhs
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


-- for types there are two different modules
-- one is that we have just parsed in, at convert time (Tree)
-- one is that we have read, and want display (Str)

-- name is not given here
data Lhs = LhsTree Constraint [String] -- context => name vars
         | LhsStr  String String
         deriving Show


data TypeVal = TypeTree TypeSig
             | TypeStr  String [String] -- context => arg0 -> arg1 .. -> argn
               deriving Show


data DataKeyword = NewTypeKeyword
                 | DataKeyword
                 deriving Enum

instance Show DataKeyword where
    show NewTypeKeyword = "newtype"
    show DataKeyword = "data"


-- note, we deliberately leave out LhsTree - it should not hit the database
instance BinaryDefer Lhs where
    bothDefer = defer [\ ~(LhsStr a b) -> unit LhsStr << a << b ]

-- note, we deliberately leave out TypeTree - it should not hit the database
instance BinaryDefer TypeVal where
    bothDefer = defer [\ ~(TypeStr a b) -> unit TypeStr << a << b ]

instance BinaryDefer DataKeyword where
    bothDefer = defer [\ ~(x@NewTypeKeyword) -> unit NewTypeKeyword <<! x
                      ,\ ~(x@DataKeyword   ) -> unit DataKeyword    <<! x ]


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


itemTreeStr :: Item -> Item
itemTreeStr x = x{itemRest = f (itemRest x)}
    where
        f (ItemClass l) = ItemClass (lhsTreeStr l)
        f (ItemFunc t) = ItemFunc (typeTreeStr t)
        f (ItemAlias l t) = ItemAlias (lhsTreeStr l) (typeTreeStr t)
        f (ItemData x l) = ItemData x (lhsTreeStr l)
        f x = x


lhsTreeStr (LhsTree x xs) = LhsStr (showConstraint x) (unwords xs)
typeTreeStr (TypeTree (TypeSig c x)) = TypeStr (showConstraint c) (map show $ splitFun x)
