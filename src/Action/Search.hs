{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Search(actionSearch, search, action_search_test) where

import System.FilePath
import Control.Monad.Extra
import qualified Data.Set as Set
import Data.List.Extra
import Data.Functor.Identity

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import General.Store
import Query
import Input.Type
import Action.CmdLine
import General.Util


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

actionSearch :: CmdLine -> IO ()
actionSearch Search{..} = do
    let pkg = [database | database /= ""]
    let rest = query
    forM_ (if null pkg then ["all"] else pkg) $ \pkg ->
        storeReadFile ("output" </> pkg <.> "hoo") $ \store -> do
            res <- return $ search store $ parseQuery $ unwords rest
            let (shown, hidden) = splitAt count $ nubOrd $ map (prettyItem . itemItem) res
            putStr $ unlines shown
            when (hidden /= []) $ do
                putStrLn $ "-- plus more results not shown, pass --count=" ++ show (count+10) ++ " to see more"


search :: StoreRead -> [Query] -> [ItemEx]
search store qs = runIdentity $ do
    let tags = readTags store
    let exact = QueryScope True "is" "exact" `elem` qs
    is <- case (filter isQueryName qs, filter isQueryType qs) of
        ([], [] ) -> return $ searchTags tags qs
        ([], t:_) -> return $ searchTypes store $ fromQueryType t
        (xs, [] ) -> return $ searchNames store exact $ map fromQueryName xs
        (xs, t:_) -> do
            nam <- return $ Set.fromList $ searchNames store exact $ map fromQueryName xs
            return $ filter (`Set.member` nam) $ searchTypes store $ fromQueryType t
    let look = lookupItem store
    return $ map look $ filter (filterTags tags qs) is


action_search_test :: IO ()
action_search_test = testing "Action.Search.search" $ storeReadFile "output/all.hoo" $ \store -> do
    let a ==$ f = do
            res <- return $ search store (parseQuery a)
            case res of
                ItemEx{..}:_ | f itemURL -> putChar '.'
                _ -> error $ show (a, take 1 res)
    let a === b = a ==$ (== b)
    let hackage x = "https://hackage.haskell.org/package/" ++ x
    "base" === hackage "base"
    "Prelude" === hackage "base/docs/Prelude.html"
    "map" === hackage "base/docs/Prelude.html#v:map"
    "map package:base" === hackage "base/docs/Prelude.html#v:map"
    "True" === hackage "base/docs/Prelude.html#v:True"
    "Bool" === hackage "base/docs/Prelude.html#t:Bool"
    "String" === hackage "base/docs/Prelude.html#t:String"
    "Ord" === hackage "base/docs/Prelude.html#t:Ord"
    ">>=" === hackage "base/docs/Prelude.html#v:-62--62--61-"
    "foldl'" === hackage "base/docs/Data-List.html#v:foldl-39-"
    "Action package:shake" === "https://hackage.haskell.org/package/shake/docs/Development-Shake.html#t:Action"
    "Action package:shake set:stackage" === "https://hackage.haskell.org/package/shake/docs/Development-Shake.html#t:Action"
    "map -package:base" ==$ \x -> not $ "/base/" `isInfixOf` x
    "<>" === hackage "base/docs/Data-Monoid.html#v:-60--62-"
    "Data.Set.insert" === hackage "containers/docs/Data-Set.html#v:insert"
    "Set.insert" === hackage "containers/docs/Data-Set.html#v:insert"
    "Prelude.mapM_" === hackage "base/docs/Prelude.html#v:mapM_"
    "Data.Complex.(:+)" === hackage "base/docs/Data-Complex.html#v::-43-"
    "\8801" === hackage "base-unicode-symbols/docs/Data-Eq-Unicode.html#v:-8801-"
    "\8484" === hackage "base-unicode-symbols/docs/Prelude-Unicode.html#t:-8484-"
    "copilot" === hackage "copilot"
    "supero" === hackage "supero"
    "set:stackage" === hackage "base"
    "author:Neil-Mitchell" === hackage "filepath"

    let tags = listTags $ readTags store
    let asserts b x = if b then putChar '.' else error $ "Assertion failed, got False for " ++ x
    asserts ("set:haskell-platform" `elem` tags) "set:haskell-platform `elem` tags"
    asserts ("author:Neil-Mitchell" `elem` tags) "author:Neil-Mitchell `elem` tags"
    asserts ("package:uniplate" `elem` tags) "package:uniplate `elem` tags"
    asserts ("package:supero" `notElem` tags) "package:supero `notElem` tags"
