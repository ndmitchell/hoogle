{-# LANGUAGE PatternGuards, ViewPatterns, CPP #-}

module General.Util(
    Score,
    fileSize,
    pretty, parseMode,
    fromName, fromTyVarBind,
    declNames,
    tarballReadFiles,
    escapeHTML,
    isUName,
    splitPair, joinPair,
    testing,
    showUTCTime,
    memoIO1, memoFile,
    error', list',
    withs,
    tag, tag_,
    ) where

import System.IO
import Language.Haskell.Exts
import Data.List.Extra
import Data.Char
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative
import Codec.Compression.GZip as GZip
import Codec.Archive.Tar as Tar
import Data.Time.Clock
import Data.Time.Format
import System.IO.Unsafe
import Control.DeepSeq
import System.Directory
import Data.IORef
#if __GLASGOW_HASKELL__< 710
import System.Locale
#endif


-- 0 is a perfect match, anything lower is less good
type Score = Double

fileSize :: FilePath -> IO Int
fileSize file = withFile file ReadMode $ fmap fromIntegral . hFileSize

pretty :: Pretty a => a -> String
pretty = trim . unwords . words . prettyPrint

parseMode :: ParseMode
parseMode = defaultParseMode{extensions=map EnableExtension es}
    where es = [ConstraintKinds,EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses
               ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples
               ,ParallelArrays,UnicodeSyntax,DataKinds,PolyKinds]


fromName :: Name -> String
fromName (Ident x) = x
fromName (Symbol x) = x

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

isUName (x:xs) = isUpper x
isUName _ = False

splitPair :: String -> String -> (String, String)
splitPair x y | (a,stripPrefix x -> Just b) <- breakOn x y = (a,b)
              | otherwise = error $ "splitPair does not contain separator " ++ show x ++ " in " ++ show y

joinPair :: [a] -> ([a], [a]) -> [a]
joinPair sep (a,b) = a ++ sep ++ b

testing :: String -> IO () -> IO ()
testing name act = do putStr $ "Test " ++ name ++ " "; act; putStrLn ""

showUTCTime :: String -> UTCTime -> String
showUTCTime = formatTime defaultTimeLocale


-- | Memoise the last result called.
memoIO1 :: Eq k => (k -> IO v) -> (k -> IO v)
memoIO1 f = unsafePerformIO $ do
    var <- newIORef Nothing
    return $ \k -> do
        val <- readIORef var
        case val of
            Just (k2,v) | k == k2 -> return v
            _ -> do
                v <- f k
                writeIORef var $ Just (k,v)
                return v

memoFile :: FilePath -> (FilePath -> IO a) -> IO a
memoFile file act = unsafePerformIO $ do
    ref <- newIORef Nothing
    return $ do
        val <- readIORef ref
        new <- getModificationTime file
        case val of
            Just (old, res) | old == new -> return res
            _ -> do res <- act file; writeIORef ref $ Just (new, res); return res


error' :: String -> a
error' msg = rnf msg `seq` error msg


list' :: NFData a => [a] -> [a]
list' (x:xs) = rnf x `seq` x : list' xs
list' [] = []


withs :: Monad m => [(a -> m r) -> m r] -> ([a] -> m r) -> m r
withs [] act = act []
withs (f:fs) act = f $ \a -> withs fs $ \as -> act $ a:as


tag :: String -> [String] -> String -> String
tag name attr inner = "<" ++ unwords (name : map f attr) ++ ">" ++ inner ++ "</" ++ name ++ ">"
    where f (break (== '=') -> (a,'=':b)) = a ++ "=\"" ++ escapeHTML b ++ "\""
          f x = x

tag_ :: String -> String -> String
tag_ name inner = tag name [] inner

