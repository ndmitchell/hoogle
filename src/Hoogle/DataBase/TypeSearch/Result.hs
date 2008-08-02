
module Hoogle.DataBase.TypeSearch.Result where

import Hoogle.DataBase.TypeSearch.TypeScore
import Hoogle.DataBase.TypeSearch.Score
import Hoogle.DataBase.Instances
import Data.Binary.Defer
import Data.Binary.Defer.Index
import Hoogle.TypeSig.All
import Hoogle.Item.All
import General.Code
import Data.Typeable
import qualified Data.IntSet as IntSet


type ArgPos = Int


-- the return from searching a graph, nearly
type Result = (Link EntryInfo,[EntryView],TypeScore)

type ResultReal = (Link Entry, [EntryView], TypeScore)


flattenResults :: [Result] -> [(Link Entry, [EntryView], TypeScore)]
flattenResults xs = [(a,b,c) | (as,b,c) <- xs, a <- entryInfoEntries $ fromLink as]


-- the information about an entry, including the arity

-- TODO: each EntryInfo should have multiple Link Entry, to account for
-- multiple Entry's with identical type signatures
data EntryInfo = EntryInfo
    {entryInfoEntries :: [Link Entry]
    ,entryInfoArity :: Int
    ,entryInfoContext :: TypeContext
    } deriving Show


typename_EntryInfo = mkTyCon "Hoogle.DataBase.TypeSearch.Result.EntryInfo"
instance Typeable EntryInfo
    where typeOf _ = mkTyConApp typename_EntryInfo []

instance BinaryDefer EntryInfo where
    put (EntryInfo a b c) = put3 a b c
    get = get3 EntryInfo


-- the result information from a whole type (many ResultArg)
-- number of lacking args, entry data, info (result:args)
data ResultAll = ResultAll Int (Link EntryInfo) [[ResultArg]]
                 deriving Show


-- the result information from one single type graph (argument/result)
-- this result points at entry.id, argument, with such a score
data ResultArg = ResultArg
    {resultArgEntry :: Link EntryInfo
    ,resultArgPos :: ArgPos
    ,resultArgScore :: TypeScore
    } deriving Show


newResultAll :: EntryInfo -> Link EntryInfo -> Maybe ResultAll
newResultAll query e
    | bad < 0 || bad > 2 = Nothing
    | otherwise = Just $ ResultAll bad e $ replicate (arityResult + 1) []
    where
        arityQuery = entryInfoArity query
        arityResult = entryInfoArity $ fromLink e
        bad = arityResult - arityQuery


addResultAll :: Instances -> EntryInfo -> (Maybe ArgPos, ResultArg) -> ResultAll -> (ResultAll, [Result])
addResultAll is query (pos,res) (ResultAll i e info) =
        (ResultAll i e info2
        ,mapMaybe (\(r:rs) -> newGraphsResults is query i e rs r) path)
    where
        ind = maybe 0 (+1) pos
        info2 = zipWith (\i x -> [res|i==ind] ++ x) [0..] info

        -- path returns a path through the ResultArg's
        -- must skip badarg items
        -- must take one element from 0
        -- must use res from ind
        path :: [[ResultArg]]
        path = f i IntSet.empty $ zip [0..] info

        f bad set [] = [[] | bad == 0]
        f bad set ((i,x):xs)
            | i == ind = map (res:) $ f bad set xs
            | i == 0 = [r:rs | r <- x, rs <- f bad set xs]
            | otherwise =
                (if bad > 0 then f (bad-1) set xs else []) ++
                [r:rs | r <- x, let rp = resultArgPos r, not $ rp `IntSet.member` set
                      , rs <- f bad (IntSet.insert rp set) xs]


newGraphsResults :: Instances -> EntryInfo -> Int -> Link EntryInfo -> [ResultArg] -> ResultArg -> Maybe Result
newGraphsResults is query badargs e args res
    | isNothing s = Nothing
    | otherwise = Just
        (e
        ,zipWith ArgPosNum [0..] $ map resultArgPos args
        ,addTypeScore (badargs * scoreDeadArg) $ fromJust s
        )
    where
        EntryInfo entry _ cresult = fromLink e
        s = mergeTypeScores is (entryInfoContext query) cresult $ map resultArgScore $ args++[res]
