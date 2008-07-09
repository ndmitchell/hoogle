
module Hoogle.DataBase.TypeSearch.Cost where

import Data.Binary.Defer
import Data.Binary.Defer.Index
import Data.Typeable
import General.Code

type CostScore = Int

data Cost = Cost CostScore CostDetail
            deriving (Ord,Eq)

typename_Cost = mkTyCon "Hoogle.DataBase.TypeSearch.Cost.Cost"
instance Typeable Cost where typeOf _ = mkTyConApp typename_Cost []

instance BinaryDefer Cost where
    put (Cost a b) = put a
    get = get1 newCost
    size _ = size (undefined :: CostDetail)
    putFixed (Cost a b) = putFixed b
    getFixed = getFixed1 newCost

instance Show Cost where
    show (Cost a b) = show b
    showList xs = showList [b | Cost a b <- xs]


data CostDetail
    = CostReverse CostDetail -- ^ Reverse/Reverse is illegal
    -- argument manipulations
    | CostDelArg -- ^ I deleted an argument from the result
    | CostArgReorder -- ^ I reordered some arguments
    -- free variables
    | CostFreeVar -- TODO: Add here
    -- standard
    | CostAlias String -- ^ a, where a |-> alias a
    | CostUnbox String -- ^ M, where M a |-> a, "" for a variable
    | CostRestrict String -- ^ M, where M |-> a -- can only restrict M :: *
    | CostContext String -- ^ C, where C a => a |-> a
    | CostMember String String -- ^ C M, where C M => M |-> C a => a
      deriving (Eq,Ord)

instance BinaryDefer CostDetail where
    put (CostReverse a)  = putByte 0 >> put1 a
    put (CostAlias a)    = putByte 1 >> put1 a
    put (CostUnbox a)    = putByte 2 >> put1 a
    put (CostRestrict a) = putByte 3 >> put1 a
    put (CostContext a)  = putByte 4 >> put1 a
    put (CostMember a b) = putByte 5 >> put2 a b

    get = do
        i <- getByte
        case i of
            0 -> get1 CostReverse
            1 -> get1 CostAlias
            2 -> get1 CostUnbox
            3 -> get1 CostRestrict
            4 -> get1 CostContext
            5 -> get2 CostMember


instance Show CostDetail where
    show (CostAlias x) = "alias " ++ x
    show (CostUnbox x) = "unbox " ++ (if null x then "_" else x)
    show (CostRestrict x) = "restrict " ++ x
    show (CostDelArg) = "delarg"

    show (CostReverse (CostUnbox x)) = "rebox " ++ (if null x then "_" else x)
    show x = "CostDetail.show.todo"


    showList = showString . concat . intersperse ", " . map show


-- transform the costCode to a costScore
newCost :: CostDetail -> Cost
newCost xs = Cost 1 xs


costScore :: Cost -> CostScore
costScore (Cost a b) = a


reverseCost :: Cost -> Maybe Cost
reverseCost (Cost _ x) | f x = Just $ newCost $ CostReverse x
                       | otherwise = Nothing
    where
        -- reverse anything aparm from a restriction
        f (CostAlias{}) = True
        f (CostUnbox{}) = True
        f (CostContext{}) = True
        f (CostMember{}) = True
        f _ = False
