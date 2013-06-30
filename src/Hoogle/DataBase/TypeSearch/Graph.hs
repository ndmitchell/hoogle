{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Search for a type signature and context through a graph.

    Return results in best-first order, taking account of which
    nodes and edges have already been paid for.
-}

module Hoogle.DataBase.TypeSearch.Graph(
    Graph, newGraph,
    graphSearch
    ) where

import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.Result
import Hoogle.Type.All
import Data.Generics.Uniplate
import Hoogle.Store.All
import qualified Data.Map as Map
import General.Base
import General.Util



newtype Graph = Graph (Map.Map Type [Node])

-- the Type's are stored in reverse, to make box/unbox computations quicker
data Node = Node [Type] [(Once EntryInfo,ArgPos)]
            deriving Typeable


instance NFData Graph where
    rnf (Graph a) = rnf a

instance NFData Node where
    rnf (Node a b) = rnf (a,b)

instance Show Graph where
    show (Graph mp) = unlines $ concatMap f $ Map.toList mp
        where f (t,ns) = show (transform g t) : map (("  "++) . show) ns
              g x = if x == TVar "" then TVar "_" else x

instance Show Node where
    show (Node t xs) = unwords $ map show t ++ "=" : ["?." ++ show b | (a,b) <- xs]


instance Store Graph where
    put (Graph a) = put1 a
    get = get1 Graph

instance Store Node where
    put (Node a b) = put2 a b
    get = get2 Node


---------------------------------------------------------------------
-- GRAPH CONSTRUCTION


typeStructure :: Type -> Type
typeStructure = transform f
    where f x = if isTLit x || isTVar x then TVar "" else x

typeUnstructure :: Type -> [Type]
typeUnstructure = reverse . filter (\x -> isTLit x || isTVar x) . universe


newGraph :: [(Once EntryInfo, ArgPos, Type)] -> Graph
newGraph = Graph . Map.map newNode . foldl' f Map.empty 
    where f mp x = Map.insertWith (++) (typeStructure $ thd3 x) [x] mp


newNode :: [(Once EntryInfo, ArgPos, Type)] -> [Node]
newNode = map (uncurry Node) . sortGroupFsts . map (\(a,b,c) -> (typeUnstructure c,(a,b)))




---------------------------------------------------------------------
-- GRAPH SEARCHING

-- must search for each (node,bindings) pair, rather than just nodes

graphSearch :: Graph -> Type -> [ResultArg]
graphSearch (Graph mp) t = [ResultArg e p b | (b,ep) <- sortFst xs, (e,p) <- ep]
    where
        xs = f newBinding s ++ f newBindingRebox (TApp (TVar "") [s]) ++
             concat [f newBindingUnbox x | TApp (TVar "") [x] <- [s]]
        u = typeUnstructure t
        s = typeStructure t

        f bind x = mapMaybe (graphCheck bind u) $ Map.findWithDefault [] x mp


graphCheck :: Binding -> [Type] -> Node -> Maybe (Binding, [(Once EntryInfo,ArgPos)])
graphCheck b xs (Node ys res) = do
    b <- f b (zip xs ys)
    return (b, res)
    where
        f b [] = Just b
        f b (x:xs) = do
            b <- addBinding x b
            f b xs
