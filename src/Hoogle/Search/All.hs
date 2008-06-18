
module Hoogle.Search.All(
    Result(..), renderResult,
    searchAll, searchRange
    ) where

import Data.Maybe
import Data.List
import Control.Monad
import General.All

import Hoogle.DataBase.All
import Hoogle.Query.All
import Hoogle.TypeSig.All



data Result = Result
    {resultDataBase :: Int
    ,resultEntry :: Entry
    ,resultModPkg :: Maybe (Module,Package)
    ,resultView :: [EntryView]
    ,resultScore :: [Score]
    }
    deriving Show

data Score = TextScore TextScore
             deriving Show

-- return the module it is in, and the text to go beside it
renderResult :: Result -> (Maybe [String], TagStr)
renderResult r = (liftM (moduleName . fst) $ resultModPkg r
                 ,renderEntryText (resultView r) (entryText $ resultEntry r))


-- return all the results
searchAll :: [DataBase] -> Query -> [Result]
searchAll databases query = getResults databases query


-- should be possible to fast-path certain searches, currently not done
searchRange :: [DataBase] -> Query -> (Int,Int) -> [Result]
searchRange databases query (from,to) =
        take (1 + to-from) $ drop from res
    where
        res = getResults databases query


getResults :: [DataBase] -> Query -> [Result]
getResults databases query = orderResults $ filterResults query res
    where
        res = if not (null $ names query) then performTextSearch databases (names query)
              else if isJust (typeSig query) then performTypeSearch databases (fromJust $ typeSig query)
              else error "Search.getResults: Doing a blank search!"


-- | Apply the PlusModule and MinusModule modes
filterResults :: Query -> [Result] -> [Result]
filterResults q xs = xs {- TODO: if null actions then xs
                     else filter (f base actions . modName . itemMod . itemResult) xs -}
    where
        actions = filter isModule $ scope q
        
        isModule (PlusModule  _) = True
        isModule (MinusModule _) = True
        isModule _ = False
        
        base = case head actions of
                    PlusModule _ -> False
                    _ -> True
        
        f z [] y = z
        f z (PlusModule  x:xs) y | doesMatch x y = f True  xs y
        f z (MinusModule x:xs) y | doesMatch x y = f False xs y
        f z (x:xs) y = f z xs y

        -- match if x is further up the tree than y
        doesMatch [] y = True
        doesMatch (x:xs) (y:ys) = x == y && doesMatch xs ys
        doesMatch _ _ = False


-- | Put the results in the correct order, by score
orderResults :: [Result] -> [Result]
orderResults = id -- TODO: map snd . sortBy (compare `on` fst) . map (\x -> (resultScore x, x))


-- | Perform a text query
performTextSearch :: [DataBase] -> [String] -> [Result]
performTextSearch databases (query:_) = concat $ zipWith f [0..] databases
    where
        f i db = [Result i e (entryParents db e) [v] [TextScore s] | (e,v,s) <- searchText db query]


performTypeSearch :: [DataBase] -> TypeSig -> [Result]
performTypeSearch databases query = [] -- TODO: concatMap (`searchType` query) databases
