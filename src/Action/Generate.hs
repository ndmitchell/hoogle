{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables, PatternGuards #-}

module Action.Generate(actionGenerate) where

import Control.Applicative
import Data.List.Extra
import System.FilePath
import System.Directory.Extra
import System.Time.Extra
import Data.Tuple.Extra
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Control.Exception.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
import Control.Monad.Extra

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
import General.Store
import System.Mem
import GHC.Stats
import Action.CmdLine


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

actionGenerate :: CmdLine -> IO ()
actionGenerate Generate{..} = do
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
                    UTF8.toString body)
        f seen _ = (seen, "")
    (seen, xs) <- second (parseHoogle . filter (/= '\r') . unlines) . mapAccumL f Set.empty <$> tarballReadFiles "input/hoogle.tar.gz"
    let out = "output" </> (if Set.size want == 1 then head $ Set.toList want else "all")
    writeStoreFile (out <.> "hoo") $ \store -> do
        xs <- reorderItems =<< writeItems store out xs
        putStrLn $ "Packages not found: " ++ unwords (Set.toList $ want `Set.difference` seen)
        writeTags store extra xs
        writeNames store xs
        writeTypes (Database out) xs

        whenM getGCStatsEnabled $ do
            performGC
            print =<< getGCStats
            void $ evaluate xs
