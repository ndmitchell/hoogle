{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables, TupleSections #-}

module Action.Search
    (actionSearch, withSearch, search
    ,targetInfo
    ,targetResultDisplay
    ,action_search_test
    ) where

import Control.DeepSeq
import Control.Monad.Extra
import Data.Functor.Identity
import Data.List.Extra
import Data.Maybe
import qualified Data.Set as Set
import System.Directory

import Action.CmdLine
import General.Store
import General.Str
import General.Util
import Input.Item
import Output.Items
import Output.Names
import Output.Tags
import Output.Types
import Query

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
            putStr $ unlines $ searchFingerprintsDebug store (parseType $ unwords query) (map parseType compare_)

-- | Returns the details printed out when hoogle --info is called
targetInfo :: Target -> String
targetInfo Target{..} =
    unlines $ [ unHTML targetItem ] ++
              [ unwords packageModule | not $ null packageModule] ++
              [ unHTML targetDocs ]
            where packageModule = map (strUnpack . fst) $ catMaybes [targetPackage, targetModule]

-- | Returns the Target formatted as an item to display in the results
-- | Bool argument decides whether links are shown
targetResultDisplay :: Bool -> Target -> String
targetResultDisplay link Target{..} = unHTML $ unwords $
        fmap (strUnpack . fst) (maybeToList targetModule) ++
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

    let top n a b = return $ isJust $ find ((== b) . targetURL) $ take n $ snd $ search store (parseQuery a)
        doesNotFind a b = not . top 1000 a b

    let query :: String -> [ExpectedQueryResult] -> IO ()
        query a qrs = let results = snd (search store (parseQuery a))
                      in forM_ qrs $ \qr -> case matchQR qr results of
                                              Success           -> putChar '.'
                                              ExpectedFailure   -> putChar 'o'
                                              _ -> error $ "Searching for: " ++ show a
                                                         ++ "\nGot: " ++ show (take 5 results)
                                                         ++ "\n expected " ++ expected qr

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
        "map is:ping" === hackage "base/docs/Prelude.html#v:map"
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
        "<>" === hackage "base/docs/Prelude.html#v:-60--62-"
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
        "author:Neil-Mitchell category:Javascript" === hackage "js-jquery"
        "( )" ==$ flip seq True -- used to segfault
        "( -is:exact) package:base=" ==$ flip seq True
        "(a -> b) -> [a] -> [b]" === hackage "base/docs/Prelude.html#v:map"
        "Ord a => [a] -> [a]" === hackage "base/docs/Data-List.html#v:sort"
        "ShakeOptions -> Int" === hackage "shake/docs/Development-Shake.html#v:shakeThreads"
        "is:module" === hackage "base/docs/Prelude.html"
        "visibleDataCons" === hackage "ghc/docs/TyCon.html#v:visibleDataCons"
        "sparkle" === hackage "sparkle" -- library without Hoogle docs
        "weeder" === hackage "weeder" -- executable in Stackage
        "supero" === hackage "supero"

        query "(a -> [a]) -> [a] -> [a]"
            [ TopHit   ("concatMap" `inModule` "GHC.OldList")
            , KnownFailure "GitHub issue #267" $
                  ("concatMap" `inModule` "Prelude") `AppearsBefore` ("concatMap" `inModule` "GHC.OldList")
            , InTop 5  ("(=<<)"      `inPackage` "base")
            , InTop 50 ("(>>=)"      `inPackage` "base")
            ]
        query "[a] -> Maybe a"
            [ TopHit  ("listToMaybe" `inModule` "Data.Maybe")
            , InTop 5 ("headMay"     `inModule` "Safe")
            ]
        query "a -> [a]"
            [ InTop 10    ("repeat"    `inPackage` "base")
            , InTop 50    ("singleton" `inModule` "Util")
            , DoesNotFind ("head"      `inPackage` "base")
            , DoesNotFind ("last"      `inPackage` "base")
            , InTop 50    ("pure"      `inPackage` "base")
            , InTop 50    ("return"    `inPackage` "base")
            , KnownFailure "GitHub issue #267" $
                  ("pure" `inPackage` "base") `AppearsBefore` ("shrinkNothing" `inModule` "Test.QuickCheck")
            , KnownFailure "GitHub issue #267" $
                  InTop 10 ("pure"   `inPackage` "base")
            , KnownFailure "GitHub issue #267" $
                  InTop 10 ("return" `inPackage` "base")
            ]
        query "[a] -> a"
            [ InTop 10 ("head" `inPackage` "base")
            , InTop 10 ("last" `inPackage` "base")
            , DoesNotFind ("repeat" `inPackage` "base")
            ]
        query "[Char] -> Char"
            [ InTop 10 ("head" `inPackage` "base")
            , RanksBelow 20 ("mconcat" `inPackage` "base")
            ]
        query "a -> b"
            [ TopHit ("unsafeCoerce" `inModule` "Unsafe.Coerce")
            , DoesNotFind ("id" `inPackage` "base") -- see GitHub issue #180
            , KnownFailure "GitHub issue #268" $
                  InTop 500 ("coerce" `inModule` "Data.Coerce")
            , KnownFailure "GitHub issue #268" $
                  InTop 5   ("coerce" `inModule` "Data.Coerce")
            ]
        query "String -> (Char -> Maybe Char) -> Maybe String" -- c/o @ndrssmn
            [ KnownFailure "GitHub issue #266" $
                  InTop 10 ("traverse" `inPackage` "base")
            , KnownFailure "GitHub issue #266" $
                  InTop 10 ("mapM" `inPackage` "base")
            , KnownFailure "GitHub issue #266" $
                  InTop 10 ("forM" `inPackage` "base")
            ]
        query "a -> [(a,b)] -> b"
            [ InTop 3 ("lookup" `inPackage` "base")
            , InTop 3 ("pairsToFunction" `inPackage` "leancheck")
            , DoesNotFind ("zip" `inPackage` "base")
            ]
        query "[(a,b)] -> a -> b"
            [ InTop 3 ("lookup" `inPackage` "base")
            , InTop 3 ("pairsToFunction" `inPackage` "leancheck")
            , DoesNotFind ("zip" `inPackage` "base")
            ]
        query "(a -> m b) -> t a -> m (t b)" -- see GitHub issue #218
            [ InTop 10 ("traverse" `inPackage` "base")
            , InTop 10 ("mapConcurrently" `inModule` "Control.Concurrent.Async.Lifted")
            , InTop 10 ("mapM" `inPackage` "base")
            , InTop 50 ("forM" `inPackage` "base")
            ]
        query "m (m a) -> m a" -- see GitHub issue #252
            [ TopHit ("join" `inPackage` "base")
            ]
        query "(a -> b -> c) -> (a -> b) -> a -> c"
            [ KnownFailure "GitHub issue #269" $
                  InTop 5 ("ap" `inPackage` "base")
            , KnownFailure "GitHub issue #269" $
                  InTop 5 ("(<*>)" `inPackage` "base")
            ]
        query "String -> Int"
            [ DoesNotFind ("cursorUpCode" `inPackage` "ansi-terminal")
            , KnownFailure "GitHub issue #266" $ InTop 20 ("length" `inPackage` "base")
            ]
        query "(a -> b) -> f a -> f b"
            [ TopHit ("fmap" `inPackage` "base")
            ]
        query "(a -> b) -> Maybe a -> Maybe b"
            [ TopHit ("fmap" `inPackage` "base")
            ]
        query "IO a -> m a" -- see GitHub issue #180
            [ InTop 3 ("liftIO" `inPackage` "base")
            ]
        query "a -> m a" -- see GitHub issue #180
            [ InTop 20 ("pure" `inPackage` "base")
            , InTop 30 ("return" `inPackage` "base")
            , KnownFailure "GitHub issue #267" $
                  InTop 3 ("pure" `inPackage` "base")
            , KnownFailure "GitHub issue #267" $
                  InTop 3 ("return" `inPackage` "base")
            ]
        query "(a -> a) -> k -> Map k a -> Map k a" -- see GitHub issue #180
            [ TopHit ("adjust" `inPackage` "containers")
            ]
        query "Int -> Integer" -- see GitHub issue #127
            [ InTop 10 ("toInteger" `inPackage` "base")
            , KnownFailure "GitHub issue #127" $
                  TopHit ("toInteger" `inPackage` "base")
            ]
        query "Integer -> Int" -- see GitHub issue #127
            [ InTop 10 ("fromInteger" `inPackage` "base")
            , KnownFailure "GitHub issue #127" $
                  TopHit ("fromInteger" `inPackage` "base")
            ]
        query "[Parser a] -> Parser a" -- see GitHub issue #90
            [ TopHit ("choice" `inPackage` "HCodecs")
            , InTop 10 ("choice" `inPackage` "attoparsec")
            ]

        let tags = completionTags store
        let asserts b x = if b then putChar '.' else error $ "Assertion failed, got False for " ++ x
        asserts ("set:haskell-platform" `elem` tags) "set:haskell-platform `elem` tags"
        asserts ("author:Neil-Mitchell" `elem` tags) "author:Neil-Mitchell `elem` tags"
        asserts ("package:uniplate" `elem` tags) "package:uniplate `elem` tags"
        asserts ("package:supero" `notElem` tags) "package:supero `notElem` tags"


--------------------------------------------------------------------------------------------------
-- Test helpers

data ExpectedQueryResult
    = TopHit TargetMatcher
    | InTop Int TargetMatcher
    | RanksBelow Int TargetMatcher
    | DoesNotFind TargetMatcher
    | AppearsBefore TargetMatcher TargetMatcher
    | NoHits
    | KnownFailure String ExpectedQueryResult

expected :: ExpectedQueryResult -> String
expected = \case
    TopHit tm       -> showTM tm ++ " as first hit."
    InTop n tm      -> showTM tm ++ " in top " ++ show n ++ " hits."
    RanksBelow n tm -> showTM tm ++ " not in top " ++ show n ++ " hits."
    DoesNotFind tm  -> "to not match " ++ showTM tm ++ "."
    AppearsBefore tm tm' -> showTM tm ++ " to appear before " ++ showTM tm' ++ "."
    NoHits          -> "no results."
    KnownFailure why qr -> "to see a failure (" ++ why ++ "): \"" ++ expected qr ++ "\" But it succeeded!"

data TestResult
    = Success
    | Failure
    | ExpectedFailure
    | UnexpectedSuccess

matchQR :: ExpectedQueryResult -> [Target] -> TestResult
matchQR qr res = case qr of
    TopHit tm        -> success $ any (runTargetMatcher tm) (take 1 res)
    InTop n tm       -> success $ any (runTargetMatcher tm) (take n res)
    RanksBelow n tm  -> success $ any (runTargetMatcher tm) (drop n res)
    DoesNotFind tm   -> success $ not $ any (runTargetMatcher tm) res
    AppearsBefore tm tm' -> success $ ( (<) <$> matchIdx tm <*> matchIdx tm' ) == Just True
    NoHits           -> success $ null res
    KnownFailure _ qr' -> case matchQR qr' res of
        Success           -> UnexpectedSuccess
        Failure           -> ExpectedFailure
        ExpectedFailure   -> Failure
        UnexpectedSuccess -> Failure
  where
    success p = if p then Success else Failure
    matchIdx tm = fmap fst $ find (runTargetMatcher tm . snd) (zip [0..] res)

data TargetMatcher
    = MatchFunctionInModule  String String
    | MatchFunctionInPackage String String

showTM :: TargetMatcher -> String
showTM = \case
    MatchFunctionInModule  f m -> m ++ "'s " ++ f
    MatchFunctionInPackage f p -> f ++ " from package " ++ p

runTargetMatcher :: TargetMatcher -> Target -> Bool
runTargetMatcher matcher Target{..} = case matcher of
    MatchFunctionInModule f m ->
        Just m == fmap (strUnpack . fst) targetModule
        && f `isPrefixOf` unHTML targetItem
    MatchFunctionInPackage f m ->
        Just m == fmap (strUnpack . fst) targetPackage
        && f `isPrefixOf` unHTML targetItem

inModule :: String -> String -> TargetMatcher
inModule = MatchFunctionInModule

inPackage :: String -> String -> TargetMatcher
inPackage = MatchFunctionInPackage

