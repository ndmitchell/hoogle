{-# LANGUAGE PatternGuards, ViewPatterns, CPP, ScopedTypeVariables, RecordWildCards #-}

module General.Util(
    URL,
    pretty, parseMode, tyApps, fromName, fromQName, fromTyVarBind, declNames,
    tarballReadFiles,
    isUpper1, isAlpha1,
    splitPair, joinPair,
    testing, timed,
    showUTCTime,
    list', strict,
    withs,
    escapeHTML, unescapeHTML, innerTextHTML, tag, tag_,
    noinline,
    takeSortOn,
    Average, toAverage, fromAverage,
    inRanges,
    readMaybe,
    exitFail,
    general_util_test
    ) where

import Control.Monad.IO.Class
import Language.Haskell.Exts
import Control.Applicative
import Data.List.Extra
import Data.Char
import Data.Either.Extra
import Data.Monoid
import Control.Monad.Extra
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Ix
import Codec.Compression.GZip as GZip
import Codec.Archive.Tar as Tar
import Data.Time.Clock
import Data.Time.Format
import Control.DeepSeq
import Control.Exception.Extra
import Test.QuickCheck
import Foreign.Storable
import Data.Int
import System.IO
import System.Exit
import System.Time.Extra
import GHC.Stats
#if __GLASGOW_HASKELL__< 710
import System.Locale
#endif
import Prelude


type URL = String


exitFail :: String -> IO ()
exitFail msg = do
    hPutStrLn stderr msg
    exitFailure

pretty :: Pretty a => a -> String
pretty = trim . unwords . words . prettyPrint

parseMode :: ParseMode
parseMode = defaultParseMode{extensions=map EnableExtension es}
    where es = [ConstraintKinds,EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses
               ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples
               ,ParallelArrays,UnicodeSyntax,DataKinds,PolyKinds]

tyApps :: Type -> [Type] -> Type
tyApps x (t:ts) = tyApps (TyApp x t) ts
tyApps x [] = x

fromName :: Name -> String
fromName (Ident x) = x
fromName (Symbol x) = x

fromQName :: QName -> String
fromQName (Qual _ x) = fromName x
fromQName (UnQual x) = fromName x
fromQName (Special UnitCon) = "()"
fromQName (Special ListCon) = "[]"
fromQName (Special FunCon) = "->"
fromQName (Special (TupleCon box n)) = "(" ++ h ++ replicate n ',' ++ h ++ ")"
    where h = ['#' | box == Unboxed]
fromQName (Special UnboxedSingleCon) = "(##)"
fromQName (Special Cons) = ":"


fromTyVarBind :: TyVarBind -> Name
fromTyVarBind (KindedVar x _) = x
fromTyVarBind (UnkindedVar x) = x

declNames :: Decl -> [String]
declNames x = map fromName $ case x of
    TypeDecl _ name _ _ -> [name]
    DataDecl _ _ _ name _ _ _ -> [name]
    GDataDecl _ _ _ name _ _ _ _ -> [name]
    TypeFamDecl _ name _ _ -> [name]
    DataFamDecl _ _ name _ _ -> [name]
    ClassDecl _ _ name _ _ _ -> [name]
    TypeSig _ names _ -> names
    _ -> []


tarballReadFiles :: FilePath -> IO [(FilePath, LBS.ByteString)]
tarballReadFiles file = f . Tar.read . GZip.decompress <$> LBS.readFile file
    where
        f (Next e rest) | NormalFile body _ <- entryContent e = (entryPath e, body) : f rest
        f (Next _ rest) = f rest
        f Done = []
        f (Fail e) = error $ "tarballReadFiles on " ++ file ++ ", " ++ show e


-- | Take a piece of text and escape all the HTML special bits
escapeHTML :: String -> String
escapeHTML = concatMap f
    where
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '&' = "&amp;"
        f '\"' = "&quot;"
        f  x  = [x]

-- | Only guarantees to be the inverse of escapeHTML
unescapeHTML :: String -> String
unescapeHTML ('&':xs)
    | Just xs <- stripPrefix "lt;" xs = '<' : unescapeHTML xs
    | Just xs <- stripPrefix "gt;" xs = '>' : unescapeHTML xs
    | Just xs <- stripPrefix "amp;" xs = '&' : unescapeHTML xs
    | Just xs <- stripPrefix "quot;" xs = '\"' : unescapeHTML xs
unescapeHTML (x:xs) = x : unescapeHTML xs
unescapeHTML [] = []

innerTextHTML :: String -> String
innerTextHTML ('<':xs) = innerTextHTML $ drop 1 $ dropWhile (/= '>') xs
innerTextHTML (x:xs) = x : innerTextHTML xs
innerTextHTML [] = []

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

timed :: MonadIO m => String -> m a -> m a
timed msg act = do
    liftIO $ putStr (msg ++ "... ") >> hFlush stdout
    time <- liftIO offsetTime
    res <- act
    time <- liftIO time
    liftIO $ putStr $ showDuration time
    liftIO $ whenM getGCStatsEnabled $ do
        stats@GCStats{..} <- getGCStats
        putStr $ " (" ++ show peakMegabytesAllocated ++ "Mb)"
    liftIO $ putStrLn ""
    return res

showUTCTime :: String -> UTCTime -> String
showUTCTime = formatTime defaultTimeLocale


list' :: NFData a => [a] -> [a]
list' (x:xs) = rnf x `seq` (x : list' xs)
list' [] = []


withs :: [(a -> r) -> r] -> ([a] -> r) -> r
withs [] act = act []
withs (f:fs) act = f $ \a -> withs fs $ \as -> act $ a:as


tag :: String -> [String] -> String -> String
tag name attr inner = "<" ++ unwords (name : map f attr) ++ ">" ++ inner ++ "</" ++ name ++ ">"
    where f (break (== '=') -> (a,'=':b)) = a ++ "=\"" ++ escapeHTML b ++ "\""
          f x = x

tag_ :: String -> String -> String
tag_ name inner = tag name [] inner

{-# NOINLINE noinline #-}
noinline :: a -> a
noinline a = a


-- ensure that no value escapes in a thunk from the value
strict :: NFData a => IO a -> IO a
strict act = do
    res <- try_ act
    case res of
        Left e -> do msg <- showException e; evaluate $ rnf msg; error msg
        Right v -> do evaluate $ rnf v; return v

-- I would like to use the storable-tuple package, but it's not in Stackage
instance Storable () where
    sizeOf _ = 0
    alignment _ = 1
    peek _ = return ()
    poke _ _ = return ()

instance forall a b . (Storable a, Storable b) => Storable (a,b) where
    sizeOf x = sizeOf (fst x) + sizeOf (snd x)
    alignment x = alignment (fst x) `max` alignment (snd x) -- dodgy, but enough for my purposes
    peekByteOff ptr pos = liftM2 (,) (peekByteOff ptr pos) (peekByteOff ptr $ pos + sizeOf (undefined :: a))
    pokeByteOff ptr pos (a,b) = pokeByteOff ptr pos a >> pokeByteOff ptr (pos + sizeOf (undefined :: a)) b


data Average a = Average !a !Int deriving Show -- a / b

toAverage :: a -> Average a
toAverage x = Average x 1

fromAverage :: Fractional a => Average a -> a
fromAverage (Average a b) = a / fromIntegral b

instance Num a => Monoid (Average a) where
    mempty = Average 0 0
    mappend (Average x1 x2) (Average y1 y2) = Average (x1+y1) (x2+y2)


readMaybe :: Read a => String -> Maybe a
readMaybe s | [x] <- [x | (x,t) <- reads s, ("","") <- lex t] = Just x
            | otherwise = Nothing


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
            | otherwise = Map.insert (fst x) (snd x) mp


general_util_test :: IO ()
general_util_test = do
    testing "General.Util.splitPair" $ do
        let a === b = if a == b then putChar '.' else error $ show (a,b)
        splitPair ":" "module:foo:bar" === ("module","foo:bar")
        do x <- try_ $ evaluate $ rnf $ splitPair "-" "module:foo"; isLeft x === True
        splitPair "-" "module-" === ("module","")
    testing_ "General.Util.inRanges" $ do
        quickCheck $ \(x :: Int8) xs -> inRanges xs x == any (`inRange` x) xs
