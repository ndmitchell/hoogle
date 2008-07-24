
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


type ArgPos = Int


-- the return from searching a graph
type Result = (Link Entry,[EntryView],TypeScore)


-- the information about an entry, including the arity
data EntryInfo = EntryInfo (Link Entry) Int TypeContext
                 deriving Show

entryInfoEntry (EntryInfo x _ _) = x
entryInfoArity (EntryInfo _ x _) = x


typename_EntryInfo = mkTyCon "Hoogle.DataBase.TypeSearch.Result.EntryInfo"
instance Typeable EntryInfo
    where typeOf _ = mkTyConApp typename_EntryInfo []

instance BinaryDefer EntryInfo where
    put (EntryInfo a b c) = put3 a b c
    get = get3 EntryInfo


-- the result information from a whole type (many ResultArg)
-- number of lacking args, entry data, info (result:args)
data ResultAll = ResultAll Int EntryInfo [[ResultArg]]


-- the result information from one single type graph (argument/result)
-- this result points at entry.id, argument, with such a score
data ResultArg = ResultArg (Link EntryInfo) ArgPos TypeScore

resultArgEntry (ResultArg x _ _) = x
resultArgPos (ResultArg _ x _) = x
resultArgScore (ResultArg _ _ x) = x


newResultAll :: Int -> EntryInfo -> Maybe ResultAll
newResultAll arityQuery e
    | bad < 0 || bad > 2 = Nothing
    | otherwise = Just $ ResultAll bad e $ replicate (entryInfoArity e + 1) []
    where bad = entryInfoArity e - arityQuery


addResultAll :: Instances -> TypeContext -> (Maybe ArgPos, ResultArg) -> ResultAll -> (ResultAll, [Result])
addResultAll is cquery (pos,res) (ResultAll i e info) =
        (ResultAll i e info2, if any null info2 then [] else catMaybes ans)
    where
        ind = maybe 0 (+1) pos
        info2 = zipWith (\i x -> [res|i==ind] ++ x) [0..] info

        ans = [ newGraphsResults is cquery i e rs r
              | (r:rs) <- sequence $ info2 !!+ (ind,[res])
              , disjoint $ map resultArgPos rs]



newGraphsResults :: Instances -> TypeContext -> Int -> EntryInfo -> [ResultArg] -> ResultArg -> Maybe Result
newGraphsResults is cquery badargs (EntryInfo entry _ cresult) args res
    | isNothing s = Nothing
    | otherwise = Just
        (entry
        ,zipWith ArgPosNum [0..] $ map resultArgPos args
        ,addTypeScore (badargs * scoreDeadArg) $ fromJust s
        )
    where
        s = mergeTypeScores is cquery cresult $ map resultArgScore $ args++[res]
