{-|
    Deal with variable bindings/alpha renaming in searches
    And with restrictions

    Deals with how the query is mapped to the result
-}

module Hoogle.DataBase.TypeSearch.Binding(
    Binding, newBinding, newBindingUnbox, newBindingRebox,
    addBinding, costBinding, costsBinding, mergeBindings, bindings
    ) where

import Hoogle.Type.All
import Hoogle.Score.All
import Data.Function
import General.Base
import qualified Data.Map as Map
import qualified Data.Set as Set


type Var = String
type Lit = String


type Bind = Map.Map Var (Maybe Lit, Set.Set Var)

data Binding = Binding !Int [Box] Bind Bind

data Box = Unbox | Rebox
           deriving (Show,Eq)


instance Show Binding where
    show b@(Binding _ box _ _) = unwords $ map (map toLower . show) box ++ map f (bindings b)
        where f (a,b) = show a ++ "=" ++ show b


instance Eq Binding where
    (==) = (==) `on` costBinding

instance Ord Binding where
    compare = comparing costBinding


costBinding :: Binding -> Int
costBinding (Binding x _ _ _) = x


newBinding, newBindingUnbox, newBindingRebox :: Binding
newBinding      = Binding 0                []      Map.empty Map.empty
newBindingUnbox = Binding (cost CostUnbox) [Unbox] Map.empty Map.empty
newBindingRebox = Binding (cost CostRebox) [Rebox] Map.empty Map.empty



costIf b v = if b then cost v else 0


addBinding :: (Type, Type) -> Binding -> Maybe Binding
addBinding (TVar a, TVar b) (Binding c box x y) = Just $ Binding c2 box x2 y2
    where (x2,cx) = addVar a b x
          (y2,cy) = addVar b a y
          c2 = c + costIf cx CostDupVarQuery + costIf cy CostDupVarResult

addBinding (TVar a, TLit b) (Binding c box x y) = do
    (x2,cx) <- addLit a b x
    return $ Binding (c + costIf cx CostRestrict) box x2 y
addBinding (TLit a, TVar b) (Binding c box x y) = do
    (y2,cy) <- addLit b a y
    return $ Binding (c + costIf cy CostUnrestrict) box x y2

addBinding (TLit a, TLit b) bind = if a == b then Just bind else Nothing


addVar :: Var -> Var -> Bind -> (Bind, Bool)
addVar a b mp = case Map.lookup a mp of
    Nothing -> (Map.insert a (Nothing, Set.singleton b) mp, False)
    Just (l, vs) | b `Set.member` vs -> (mp, False)
                 | otherwise -> (Map.insert a (l, Set.insert b vs) mp, True)


addLit :: Var -> Lit -> Bind -> Maybe (Bind, Bool)
addLit a b mp | l == Just b = Just (mp, False)
              | isJust l = Nothing
              | otherwise = Just (Map.insert a (Just b, vs) mp, True)
    where (l, vs) = Map.findWithDefault (Nothing, Set.empty) a mp



mergeBindings :: [Binding] -> Maybe Binding
mergeBindings bs = do
    let (box,ls,rs) = unzip3 [(b,l,r) | Binding _ b l r <- bs]
        (bl,br) = (Map.unionsWith f ls, Map.unionsWith f rs)
        res i = Binding i (concat box) bl br
    s <- costsBindingLocal (res 0)
    return $ res (sum $ map cost s)
    where
        f (l1,vs1) (l2,vs2)
            | l1 /= l2 && isJust l1 && isJust l2 = (Just "", vs1)
            | otherwise = (l1 `mplus` l2, Set.union vs1 vs2)


costsBindingLocal :: Binding -> Maybe [TypeCost]
costsBindingLocal (Binding _ box l r) = do
    let cb = [if b == Unbox then CostUnbox else CostRebox | b <- box]
    cl <- f CostDupVarQuery  CostRestrict   l
    cr <- f CostDupVarResult CostUnrestrict r
    return $ cb++cl++cr
    where
        f var restrict = concatMapM g . Map.elems
            where
                g (Just "", _) = Nothing
                g (l, vs) = Just $ [restrict|isJust l] ++ replicate (max 0 $ Set.size vs - 1) var


costsBinding :: Binding -> [TypeCost]
costsBinding = fromJust . costsBindingLocal


bindings :: Binding -> [(Type, Type)]
bindings (Binding _ _ a b) =
    [(TVar v, t) | (v,(l,vs)) <- Map.toList a, t <- [TLit l | Just l <- [l]] ++ map TVar (Set.toList vs)] ++
    [(TLit l, TVar v) | (v,(Just l,_)) <- Map.toList b]
