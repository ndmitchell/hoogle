{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Action.Search(actionSearch, withSearch, search,
                    targetInfo,
                    targetResultDisplay,
                    action_search_test) where

import Control.Monad.Extra
import Control.DeepSeq
import Data.Maybe
import qualified Data.Set as Set
import Data.List.Extra
import Data.Functor.Identity
import System.Directory

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import General.Store
import Query
import Input.Item
import Action.CmdLine
import General.Util

-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

actionSearch :: CmdLine -> IO ()
actionSearch Search{..} = replicateM_ repeat_ $ -- deliberately reopen the database each time
    withSearch database $ \store ->
        if null compare_ then do
            (q, res) <- return $ search store $ parseQuery $ unwords query
            whenLoud $ putStrLn $ "Query: " ++ unescapeHTML (renderQuery q)
            let (shown, hidden) = splitAt count $ nubOrd $ map (targetResultDisplay link) res
            if null res then
                putStrLn "No results found"
             else if info then do
                 putStr $ targetInfo $ head res
             else do
                let toShow = if numbers && not info then addCounter shown else shown
                putStr $ unlines toShow
                when (hidden /= []) $ do
                    whenNormal $ putStrLn $ "-- plus more results not shown, pass --count=" ++ show (count+10) ++ " to see more"
        else do
            let parseType x = case parseQuery x of
                                  [QueryType t] -> (pretty t, hseToSig t)
                                  _ -> error $ "Expected a type signature, got: " ++ x
            putStr $ unlines $ searchTypesDebug store (parseType $ unwords query) (map parseType compare_)

-- | Returns the details printed out when hoogle --info is called
targetInfo :: Target -> String
targetInfo Target{..} = 
    unlines $ [ unHTML targetItem ] ++
              [ unwords packageModule | not $ null packageModule] ++
              [ unHTML targetDocs ]
            where packageModule = map fst $ catMaybes [targetPackage, targetModule]

-- | Returns the Target formatted as an item to display in the results
-- | Bool argument decides whether links are shown
targetResultDisplay :: Bool -> Target -> String
targetResultDisplay link Target{..} = unHTML $ unwords $
        fmap fst (maybeToList targetModule) ++
        [targetItem] ++
        ["-- " ++ targetURL | link]

addCounter :: [String] -> [String]
addCounter = zipWith (\i x -> show i ++ ") " ++ x) [1..]

withSearch :: NFData a => FilePath -> (StoreRead -> IO a) -> IO a
withSearch database act = do
    unlessM (doesFileExist database) $ do
        exitFail $ "Error, database does not exist (run 'hoogle generate' first)\n" ++
                   "    Filename: " ++ database
    storeReadFile database act


search :: StoreRead -> [Query] -> ([Query], [Target])
search store qs = runIdentity $ do
    (qs, exact, filt, list) <- return $ applyTags store  qs
    is <- case (filter isQueryName qs, filter isQueryType qs) of
        ([], [] ) -> return list
        ([], t:_) -> return $ searchTypes store $ hseToSig $ fromQueryType t
        (xs, [] ) -> return $ searchNames store exact $ map fromQueryName xs
        (xs, t:_) -> do
            nam <- return $ Set.fromList $ searchNames store exact $ map fromQueryName xs
            return $ filter (`Set.member` nam) $ searchTypes store $ hseToSig $ fromQueryType t
    let look = lookupItem store
    return (qs, map look $ filter filt is)


action_search_test :: Bool -> FilePath -> IO ()
action_search_test sample database = testing "Action.Search.search" $ withSearch database $ \store -> do
    let noResults a = do
          res <- return $ snd $ search store (parseQuery a)
          case res of
              [] -> putChar '.'
              _ -> error $ "Searching for: " ++ show a ++ "\nGot: " ++ show (take 1 res) ++ "\n expected none"
    let a ==$ f = do
            res <- return $ snd $ search store (parseQuery a)
            case res of
                Target{..}:_ | f targetURL -> putChar '.'
                _ -> error $ "Searching for: " ++ show a ++ "\nGot: " ++ show (take 1 res)
    let a === b = a ==$ (== b)
    let hackage x = "https://hackage.haskell.org/package/" ++ x
    if sample then do
        "__prefix__" === "http://henry.com?too_long"
        "__suffix__" === "http://henry.com?too_long"
        "__infix__" === "http://henry.com?too_long"
        "Wife" === "http://eghmitchell.com/Mitchell.html#a_wife"
        completionTags store `testEq` ["set:all","package:emily","package:henry"]
     else do
        "base" === hackage "base"
        "Prelude" === hackage "base/docs/Prelude.html"
        "map" === hackage "base/docs/Prelude.html#v:map"
        "map package:base" === hackage "base/docs/Prelude.html#v:map"
        noResults "map package:package-not-in-db"
        noResults "map module:Module.Not.In.Db"
        "True" === hackage "base/docs/Prelude.html#v:True"
        "Bool" === hackage "base/docs/Prelude.html#t:Bool"
        "String" === hackage "base/docs/Prelude.html#t:String"
        "Ord" === hackage "base/docs/Prelude.html#t:Ord"
        ">>=" === hackage "base/docs/Prelude.html#v:-62--62--61-"
        "sequen" === hackage "base/docs/Prelude.html#v:sequence"
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
        -- FIXME: "author:Neil-M" === hackage "filepath"
        -- FIXME: "Data.Se.insert" === hackage "containers/docs/Data-Set.html#v:insert"
        "set:-haskell-platform author:Neil-Mitchell" === hackage "safe"
        "author:Neil-Mitchell category:Development" === hackage "hlint"
        "( )" ==$ flip seq True -- used to segfault
        "( -is:exact) package:base=" ==$ flip seq True
        "(a -> b) -> [a] -> [b]" === hackage "base/docs/Prelude.html#v:map"
        "Ord a => [a] -> [a]" === hackage "base/docs/Data-List.html#v:sort"
        "ShakeOptions -> Int" === hackage "shake/docs/Development-Shake.html#v:shakeThreads"
        "is:module" === hackage "base/docs/Prelude.html"
        "visibleDataCons" === hackage "ghc/docs/TyCon.html#v:visibleDataCons"

        let tags = completionTags store
        let asserts b x = if b then putChar '.' else error $ "Assertion failed, got False for " ++ x
        asserts ("set:haskell-platform" `elem` tags) "set:haskell-platform `elem` tags"
        asserts ("author:Neil-Mitchell" `elem` tags) "author:Neil-Mitchell `elem` tags"
        asserts ("package:uniplate" `elem` tags) "package:uniplate `elem` tags"
        asserts ("package:supero" `notElem` tags) "package:supero `notElem` tags"
