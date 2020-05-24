{-# LANGUAGE PatternGuards, ViewPatterns, CPP, ScopedTypeVariables #-}

module General.Util(
    PkgName, ModName,
    URL,
    pretty, parseMode, applyType, applyFun1, unapplyFun, fromName, fromQName, fromTyVarBind, declNames, isTypeSig,
    fromDeclHead, fromContext, fromIParen, fromInstHead,
    tarballReadFiles,
    isUpper1, isAlpha1,
    joinPair,
    testing, testEq,
    showUTCTime,
    strict,
    withs,
    escapeHTML, unescapeHTML, unHTML,
    escapeURL,
    takeSortOn,
    Average, toAverage, fromAverage,
    inRanges,
    parseTrailingVersion,
    trimVersion,
    exitFail,
    prettyTable,
    getStatsPeakAllocBytes, getStatsCurrentLiveBytes, getStatsDebug,
    hackagePackageURL, hackageModuleURL, hackageDeclURL, ghcModuleURL,
    minimum', maximum',
    general_util_test
    ) where

import Language.Haskell.Exts
import Control.Applicative
import Data.List.Extra
import Data.Char
import Data.Either.Extra
import Data.Semigroup
import Data.Tuple.Extra
import Control.Monad.Extra
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Ix
import Numeric.Extra
import Codec.Compression.GZip as GZip
import Codec.Archive.Tar as Tar
import Data.Time.Clock
import Data.Time.Format
import Control.DeepSeq
import Control.Exception.Extra
import Test.QuickCheck
import Data.Version
import Data.Int
import System.IO
import System.Exit
import System.Mem
import GHC.Stats
import General.Str
import Prelude
import qualified Network.HTTP.Types.URI as URI
import qualified Data.ByteString.UTF8 as UTF8


type PkgName = Str
type ModName = Str

-- | A URL, complete with a @https:@ prefix.
type URL = String

#if __GLASGOW_HASKELL__ >= 802
#define RTS_STATS 1
#endif

showMb :: (Show a, Integral a) => a -> String
#if RTS_STATS
showMb x = show (x `div` (1024*1024)) ++ "Mb"
#else
showMb x = show x ++ "Mb"
#endif


#if RTS_STATS
withRTSStats :: (RTSStats -> a) -> IO (Maybe a)
withRTSStats f = ifM getRTSStatsEnabled (Just . f <$> getRTSStats) (pure Nothing)
#else
withGCStats :: (GCStats -> a) -> IO (Maybe a)
withGCStats f = ifM getGCStatsEnabled (Just . f <$> getGCStats) (pure Nothing)
#endif

getStatsCurrentLiveBytes :: IO (Maybe String)
getStatsCurrentLiveBytes = do
    performGC
#if RTS_STATS
    withRTSStats $ showMb . gcdetails_live_bytes . gc
#else
    withGCStats $ showMb . currentBytesUsed
#endif

getStatsPeakAllocBytes :: IO (Maybe String)
getStatsPeakAllocBytes = do
#if RTS_STATS
    withRTSStats $ showMb . max_mem_in_use_bytes
#else
    withGCStats $ showMb . peakMegabytesAllocated
#endif

getStatsDebug :: IO (Maybe String)
getStatsDebug = do
    let dump = replace ", " "\n" . takeWhile (/= '}') . drop1 . dropWhile (/= '{') . show
#if RTS_STATS
    withRTSStats dump
#else
    withGCStats dump
#endif



exitFail :: String -> IO ()
exitFail msg = do
    hPutStrLn stderr msg
    exitFailure

pretty :: Pretty a => a -> String
pretty = prettyPrintWithMode defaultMode{layout=PPNoLayout}


parseMode :: ParseMode
parseMode = defaultParseMode{extensions=map EnableExtension es}
    where es = [ConstraintKinds,EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses
               ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples
               ,ParallelArrays,UnicodeSyntax,DataKinds,PolyKinds,PatternSynonyms]

applyType :: Type a -> [Type a] -> Type a
applyType x (t:ts) = applyType (TyApp (ann t) x t) ts
applyType x [] = x

applyFun1 :: [Type a] -> Type a
applyFun1 [x] = x
applyFun1 (x:xs) = TyFun (ann x) x $ applyFun1 xs

unapplyFun :: Type a -> [Type a]
unapplyFun (TyFun _ x y) = x : unapplyFun y
unapplyFun x = [x]


fromName :: Name a -> String
fromName (Ident _ x) = x
fromName (Symbol _ x) = x

fromQName :: QName a -> String
fromQName (Qual _ _ x) = fromName x
fromQName (UnQual _ x) = fromName x
fromQName (Special _ UnitCon{}) = "()"
fromQName (Special _ ListCon{}) = "[]"
fromQName (Special _ FunCon{}) = "->"
fromQName (Special _ (TupleCon _ box n)) = "(" ++ h ++ replicate n ',' ++ h ++ ")"
    where h = ['#' | box == Unboxed]
fromQName (Special _ UnboxedSingleCon{}) = "(##)"
fromQName (Special _ Cons{}) = ":"

fromContext :: Context a -> [Asst a]
fromContext (CxSingle _ x) = [x]
fromContext (CxTuple _ xs) = xs
fromContext _ = []

fromIParen :: InstRule a -> InstRule a
fromIParen (IParen _ x) = fromIParen x
fromIParen x = x

fromTyVarBind :: TyVarBind a -> Name a
fromTyVarBind (KindedVar _ x _) = x
fromTyVarBind (UnkindedVar _ x) = x

fromDeclHead :: DeclHead a -> (Name a, [TyVarBind a])
fromDeclHead (DHead _ n) = (n, [])
fromDeclHead (DHInfix _ x n) = (n, [x])
fromDeclHead (DHParen _ x) = fromDeclHead x
fromDeclHead (DHApp _ dh x) = second (++[x]) $ fromDeclHead dh

fromInstHead :: InstHead a -> (QName a, [Type a])
fromInstHead (IHCon _ n) = (n, [])
fromInstHead (IHInfix _ x n) = (n, [x])
fromInstHead (IHParen _ x) = fromInstHead x
fromInstHead (IHApp _ ih x) = second (++[x]) $ fromInstHead ih

declNames :: Decl a -> [String]
declNames x = map fromName $ case x of
    TypeDecl _ hd _ -> f hd
    DataDecl _ _ _ hd _ _ -> f hd
    GDataDecl _ _ _ hd _ _ _ -> f hd
    TypeFamDecl _ hd _ _ -> f hd
    DataFamDecl _ _ hd _ -> f hd
    ClassDecl _ _ hd _ _ -> f hd
    TypeSig _ names _ -> names
    PatSynSig _ names _ _ _ _ _ -> names
    _ -> []
    where f x = [fst $ fromDeclHead x]


isTypeSig :: Decl a -> Bool
isTypeSig TypeSig{} = True
isTypeSig PatSynSig{} = True
isTypeSig _ = False


tarballReadFiles :: FilePath -> IO [(FilePath, LBS.ByteString)]
tarballReadFiles file = f . Tar.read . GZip.decompress <$> LBS.readFile file
    where
        f (Next e rest) | NormalFile body _ <- entryContent e = (entryPath e, body) : f rest
        f (Next _ rest) = f rest
        f Done = []
        f (Fail e) = error $ "tarballReadFiles on " ++ file ++ ", " ++ show e


innerTextHTML :: String -> String
innerTextHTML ('<':xs) = innerTextHTML $ drop1 $ dropWhile (/= '>') xs
innerTextHTML (x:xs) = x : innerTextHTML xs
innerTextHTML [] = []

unHTML :: String -> String
unHTML = unescapeHTML . innerTextHTML

escapeURL :: String -> String
escapeURL = UTF8.toString . URI.urlEncode True . UTF8.fromString

isUpper1 (x:xs) = isUpper x
isUpper1 _ = False

isAlpha1 (x:xs) = isAlpha x
isAlpha1 [] = False

splitPair :: String -> String -> (String, String)
splitPair x y | (a,stripPrefix x -> Just b) <- breakOn x y = (a,b)
              | otherwise = error $ "splitPair does not contain separator " ++ show x ++ " in " ++ show y

joinPair :: [a] -> ([a], [a]) -> [a]
joinPair sep (a,b) = a ++ sep ++ b

testing_, testing :: String -> IO () -> IO ()
testing_ name act = do putStr $ "Test " ++ name ++ " "; act
testing name act = do testing_ name act; putStrLn ""

testEq :: (Show a, Eq a) => a -> a -> IO ()
testEq a b | a == b = putStr "."
           | otherwise = errorIO $ "Expected equal, but " ++ show a ++ " /= " ++ show b

showUTCTime :: String -> UTCTime -> String
showUTCTime = formatTime defaultTimeLocale


withs :: [(a -> r) -> r] -> ([a] -> r) -> r
withs [] act = act []
withs (f:fs) act = f $ \a -> withs fs $ \as -> act $ a:as


prettyTable :: Int -> String -> [(String, Double)] -> [String]
prettyTable dp units xs =
    ( padR len units ++ "\tPercent\tName") :
    [ padL len (showDP dp b) ++ "\t" ++ padL 7 (showDP 1 (100 * b / tot) ++ "%") ++ "\t" ++ a
    | (a,b) <- ("Total", tot) : sortOn (negate . snd) xs]
    where
        tot = sum $ map snd xs
        len = length units `max` length (showDP dp tot)

        padL n s = replicate (n - length s) ' ' ++ s
        padR n s = s ++ replicate (n - length s) ' '

-- ensure that no value escapes in a thunk from the value
strict :: NFData a => IO a -> IO a
strict act = do
    res <- try_ act
    case res of
        Left e -> do msg <- showException e; evaluate $ rnf msg; errorIO msg
        Right v -> evaluate $ force v

data Average a = Average !a {-# UNPACK #-} !Int deriving Show -- a / b

toAverage :: a -> Average a
toAverage x = Average x 1

fromAverage :: Fractional a => Average a -> a
fromAverage (Average a b) = a / fromIntegral b

instance Num a => Semigroup (Average a) where
    Average x1 x2 <> Average y1 y2 = Average (x1+y1) (x2+y2)

instance Num a => Monoid (Average a) where
    mempty = Average 0 0
    mappend = (<>)


data TakeSort k v = More !Int !(Map.Map k [v])
                  | Full !k !(Map.Map k [v])

-- | @takeSortOn n op == take n . sortOn op@
takeSortOn :: Ord k => (a -> k) -> Int -> [a] -> [a]
takeSortOn op n xs
    | n <= 0 = []
    | otherwise = concatMap reverse $ Map.elems $ getMap $ foldl' add (More n Map.empty) xs
    where
        getMap (More _ mp) = mp
        getMap (Full _ mp) = mp

        add (More n mp) x = (if n <= 1 then full else More (n-1)) $ Map.insertWith (++) (op x) [x] mp
        add o@(Full mx mp) x = let k = op x in if k >= mx then o else full $ Map.insertWith (++) k [x] $ delMax mp
        full mp = Full (fst $ Map.findMax mp) mp
        delMax mp | Just ((k,_:vs), mp) <- Map.maxViewWithKey mp = if null vs then mp else Map.insert k vs mp



-- See https://ghc.haskell.org/trac/ghc/ticket/10830 - they broke maximumBy
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' cmp = foldl1' $ \x y -> if cmp x y == GT then x else y

maximum' :: Ord a => [a] -> a
maximum' = maximumBy' compare

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' cmp = foldl1' $ \x y -> if cmp x y == LT then x else y

minimum' :: Ord a => [a] -> a
minimum' = minimumBy' compare


hackagePackageURL :: PkgName -> URL
hackagePackageURL x = "https://hackage.haskell.org/package/" ++ strUnpack x

hackageModuleURL :: ModName -> URL
hackageModuleURL x = "/docs/" ++ ghcModuleURL x

ghcModuleURL :: ModName -> URL
ghcModuleURL x = replace "." "-" (strUnpack x) ++ ".html"

hackageDeclURL :: Bool -> String -> URL
hackageDeclURL typesig x = "#" ++ (if typesig then "v" else "t") ++ ":" ++ concatMap f x
    where
        f x | isLegal x = [x]
            | otherwise = "-" ++ show (ord x) ++ "-"
        -- isLegal is from haddock-api:Haddock.Utils; we need to use
        -- the same escaping strategy here in order for fragment links
        -- to work
        isLegal ':' = True
        isLegal '_' = True
        isLegal '.' = True
        isLegal c = isAscii c && isAlphaNum c


trimVersion :: Int -> Version -> Version
trimVersion i v = v{versionBranch = take 3 $ versionBranch v}

parseTrailingVersion :: String -> (String, [Int])
parseTrailingVersion = (reverse *** reverse) . f . reverse
    where
        f xs | (ver@(_:_),sep:xs) <- span isDigit xs
             , sep == '-' || sep == '.'
             , (a, b) <- f xs
             = (a, Prelude.read (reverse ver) : b)
        f xs = (xs, [])


-- | Equivalent to any (`inRange` x) xs, but more efficient
inRanges :: Ix a => [(a,a)] -> (a -> Bool)
inRanges xs = \x -> maybe False (`inRange` x) $ Map.lookupLE x mp
    where
        mp = foldl' add Map.empty xs

        merge (l1,u1) (l2,u2) = (min l1 l2, max u1 u2)
        overlap x1 x2 = x1 `inRange` fst x2 || x2 `inRange` fst x1
        add mp x
            | Just x2 <- Map.lookupLE (fst x) mp, overlap x x2 = add (Map.delete (fst x2) mp) (merge x x2)
            | Just x2 <- Map.lookupGE (fst x) mp, overlap x x2 = add (Map.delete (fst x2) mp) (merge x x2)
            | otherwise = uncurry Map.insert x mp


general_util_test :: IO ()
general_util_test = do
    testing "General.Util.splitPair" $ do
        let a === b = if a == b then putChar '.' else errorIO $ show (a,b)
        splitPair ":" "module:foo:bar" === ("module","foo:bar")
        do x <- try_ $ evaluate $ rnf $ splitPair "-" "module:foo"; isLeft x === True
        splitPair "-" "module-" === ("module","")
    testing_ "General.Util.inRanges" $ do
        quickCheck $ \(x :: Int8) xs -> inRanges xs x == any (`inRange` x) xs
    testing "General.Util.parseTrailingVersion" $ do
        let a === b = if a == b then putChar '.' else errorIO $ show (a,b)
        parseTrailingVersion "shake-0.15.2" === ("shake",[0,15,2])
        parseTrailingVersion "test-of-stuff1" === ("test-of-stuff1",[])
