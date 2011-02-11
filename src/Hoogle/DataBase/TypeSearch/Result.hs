
module Hoogle.DataBase.TypeSearch.Result(
    module Hoogle.DataBase.TypeSearch.Result,
    module Hoogle.DataBase.TypeSearch.EntryInfo
    ) where

import Hoogle.DataBase.TypeSearch.TypeScore
import Hoogle.DataBase.TypeSearch.Binding
import Hoogle.DataBase.TypeSearch.EntryInfo
import Hoogle.DataBase.Instances
import Hoogle.Type.All hiding (Result)
import General.Base
import Hoogle.Store.All
import qualified Data.IntSet as IntSet


type ArgPos = Int


-- the return from searching a graph, nearly
type Result = (Once EntryInfo,[EntryView],TypeScore)

type ResultReal = (Once Entry, [EntryView], TypeScore)


flattenResults :: [Result] -> [(Once Entry, [EntryView], TypeScore)]
flattenResults xs = [(a,b,c) | (as,b,c) <- xs, a <- entryInfoEntries $ fromOnce as]


-- the result information from a whole type (many ResultArg)
-- number of lacking args, entry data, info (result:args)
data ResultAll = ResultAll Int (Once EntryInfo) [[ResultArg]]
                 deriving Show


-- the result information from one single type graph (argument/result)
-- this result points at entry.id, argument, with such a score
data ResultArg = ResultArg
    {resultArgEntry :: Once EntryInfo
    ,resultArgPos :: ArgPos
    ,resultArgBind :: Binding
    } deriving Show


newResultAll :: EntryInfo -> Once EntryInfo -> Maybe ResultAll
newResultAll query e
    | bad < 0 || bad > 2 = Nothing
    | otherwise = Just $ ResultAll bad e $ replicate (arityResult + 1) []
    where
        arityQuery = entryInfoArity query
        arityResult = entryInfoArity $ fromOnce e
        bad = arityResult - arityQuery


addResultAll :: Instances -> EntryInfo -> (Maybe ArgPos, ResultArg) -> ResultAll -> (ResultAll, [Result])
addResultAll is query (pos,res) (ResultAll i e info) =
        (ResultAll i e info2
        ,mapMaybe (\(r:rs) -> newGraphsResults is query e rs r) path)
    where
        ind = maybe 0 (+1) pos
        info2 = zipWith (\i x -> [res|i==ind] ++ x) [0..] info

        -- path returns a path through the ResultArg's
        -- must skip badarg items
        -- must take one element from 0
        -- must use res from ind
        path :: [[ResultArg]]
        path = f i set $ zip [0..] info
            where set = if ind == 0 then IntSet.empty else IntSet.singleton (resultArgPos res)

        f bad set [] = [[] | bad == 0]
        f bad set ((i,x):xs)
            | i == ind = map (res:) $ f bad set xs
            | i == 0 = [r:rs | r <- x, rs <- f bad set xs]
            | otherwise =
                (if bad > 0 then f (bad-1) set xs else []) ++
                [r:rs | r <- x, let rp = resultArgPos r, not $ rp `IntSet.member` set
                      , rs <- f bad (IntSet.insert rp set) xs]


newGraphsResults :: Instances -> EntryInfo -> Once EntryInfo -> [ResultArg] -> ResultArg -> Maybe Result
newGraphsResults is query e args res = do
    b <- mergeBindings $ map resultArgBind $ args ++ [res]
    let aps = map resultArgPos args
        s = newTypeScore is query (fromOnce e) (aps == sort aps) b
        view = zipWith ArgPosNum [0..] aps
        -- need to fake at least one ArgPosNum, so we know we have some highlight info
        view2 = [ArgPosNum (-1) (-1) | null view] ++ view
    return (e, view2, s)
