{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Generate(generateMain) where

import Control.Applicative
import Data.List.Extra
import System.FilePath
import System.Directory.Extra
import System.Time.Extra
import Data.Tuple.Extra
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Exception.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

import Output.Items
import Output.Tags
import Output.Names
import Output.Types
import Input.Cabal
import Input.Hoogle
import Input.Download
import Input.Reorder
import Input.Set
import Input.Type
import General.Util
import System.Mem
import GHC.Stats
import Action.CmdLine


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

generateMain :: CmdLine -> IO ()
generateMain Generate{..} = do
    downloadInputs
    (n,_) <- duration $ generate include
    putStrLn $ "Took " ++ showDuration n


generate :: [String] -> IO ()
generate xs = do
    setStackage <- setStackage
    setPlatform <- setPlatform
    setGHC <- setGHC
    createDirectoryIfMissing True "output"
    let want = Set.fromList $ if null xs then setStackage else xs

    cbl <- parseCabal (`Set.member` want)
    let extra pkg = [("set","included-with-ghc") | pkg `elem` setGHC] ++
                    [("set","haskell-platform") | pkg `elem` setPlatform] ++
                    [("set","stackage")] ++
                    Map.findWithDefault [] pkg cbl

    let f seen (takeBaseName -> pkg, body)
            | pkg `Set.member` want
            = (Set.insert pkg seen,
                trace ("[" ++ show (Set.size seen + 1) ++ "/" ++ show (Set.size want) ++ "] " ++ pkg) $
                    LBS.unpack body)
        f seen _ = (seen, "")
    (seen, xs) <- second (parseHoogle . unlines) . mapAccumL f Set.empty <$> tarballReadFiles "input/hoogle.tar.gz"
    let out = "output" </> (if Set.size want == 1 then head $ Set.toList want else "all")
--    xs <- writeFileLefts (out <.> "warn") xs
    xs <- reorderItems =<< writeItems out [x | Right x <- xs]
    putStrLn $ "Packages not found: " ++ unwords (Set.toList $ want `Set.difference` seen)
    writeTags (Database out) extra xs
    writeNames (Database out) xs
    writeTypes (Database out) xs

    performGC
    print =<< getGCStats
    evaluate xs
    print "done"

{-
writeFileLefts :: FilePath -> [Either String a] -> IO [a]
writeFileLefts file xs = do
    ignore $ removeFile file
    f Nothing xs
    where
        f Nothing xs@(Left _:_) = do h <- openBinaryFile file WriteMode; f (Just h) xs
        f (Just h) (Left x:xs) = do res <- unsafeInterleaveIO $ hPutStrLn h x; res `seq` f (Just h) xs
        f h (Right x:xs) = fmap (x:) $ f h xs
        f h [] = do whenJust h hClose; return []
-}
