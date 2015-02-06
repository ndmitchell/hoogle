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
            forM_ (maybe id take count res) $ putStrLn . prettyItem . itemItem

search :: StoreRead -> Query -> [ItemEx]
search store (Query strs typ qtags) = runIdentity $ do
    let tags = readTags store
    let exact = Scope True "is" "exact" `elem` qtags
    is <- case (strs, typ) of
        ([], Nothing) -> return $ searchTags tags qtags
        ([], Just t ) -> return $ searchTypes store t
        (xs, Nothing) -> return $ searchNames store exact xs
        (xs, Just t ) -> do
            nam <- return $ Set.fromList $ searchNames store exact xs
            return $ filter (`Set.member` nam) $ searchTypes store t
    let look = lookupItem store
    return $ map (look . snd) $ sortOn fst $ filter (filterTags tags qtags . snd) is


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
