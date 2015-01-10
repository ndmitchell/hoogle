{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- grp = 1.28Mb
-- wrd = 10.7Mb

-- [(".grp",1_343_808),(".ids",247_336_269),(".wrd",11_230_952)]
-- [(".grp",1_314_256),(".ids",244_154_208),(".wrd",7_369_220)]


import Language.Haskell.Exts.Annotated
import Control.Applicative
import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Control.Monad
import System.Directory.Extra
import System.Time.Extra
import Data.Tuple.Extra
import Data.Either
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Generics.Uniplate.Data
import Data.Char

import DataTags
import ParseHoogle
import ParseQuery
import Type
import Util


-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

main :: IO ()
main = do
    args <- getArgs
    let (pkg,rest) = first (map tail) $ span ("@" `isPrefixOf`) args
    if null rest then
        generate pkg
     else do
        let q = parseQuery $ unwords rest
        if null pkg then search "all" q else mapM_ (`search` q) pkg


search :: String -> Query -> IO ()
search pkgs (Query [] [] Nothing) = putStrLn "No search entered, nothing to do"
search pkgs q@(Query tags strs typ) = error $ show q


searchName :: String -> String -> IO [Id]
searchName = error "searchname"

searchType :: String -> Type () -> IO [Id]
searchType = error "searchType"


generate :: [String] -> IO ()
generate xs = do
    files <- if xs /= [] then return ["input/hoogle" </> x <.> "txt" | x <- xs] else do
        files <- lines <$> readFile' "input/stackage.txt"
        filterM doesFileExist ["input/hoogle" </> x <.> "txt" | x <- files]
    let n = length files
    forM_ (zip [1..] files) $ \(i,file) -> do
        let out = "output" </> takeBaseName file
        putStr $ "[" ++ show i ++ "/" ++ show n ++ "] " ++ takeBaseName file
        (t,_) <- duration $ do
            src <- readFile' file
            (warns, xs) <- return $ partitionEithers $ parseHoogle src
            unless (null warns) $ writeFile (out <.> "warn") $ unlines warns
            xs <- allocIdentifiers out xs
            writeGroups out xs
            searchNames out xs
            searchTypes out xs
        putStrLn $ " in " ++ show (round t) ++ "s"
    files <- listFiles "output"
    files <- forM files $ \file -> (takeExtension file,) <$> fileSize file
    print $ map (second sum) $ groupSort files
    print "done"


experiment :: IO ()
experiment = do
    files <- listFiles "output"
    types <- fmap concat $ forM (filter ((==) ".types" . takeExtension) files) $ \file -> do
        xs <- readFile' file
        return [x | ParseOk x <- map (parseType . snd . word1) $ lines xs]
    print ("Count", length types)
    print ("Unique", Set.size $ Set.fromList types)
    let disp = trimStart . unwords . words . prettyPrint
    writeFileBinary "types.txt" $ unlines $ map disp $ Set.toList $ Set.fromList types
    writeFileBinary "ctors.txt" $ unlines $ map show $ reverse $ sortOn snd $ Map.toList $ Map.fromListWith (+) $ concat [nub [(x:xs,1) | Ident (_ :: SrcSpanInfo) (x:xs) <- universeBi t, isUpper x] | t <- Set.toList $ Set.fromList types]
    writeFileBinary "contexts.txt" $ unlines [disp t | t <- Set.toList $ Set.fromList types, any ((>1) . length . snd) $ groupSort [(prettyPrint v,cls) | ClassA (_ :: SrcSpanInfo) cls [v] <- universeBi t]]
    error "done"

    src <- BS.readFile "output/bullet.ids"
    forM_ ["Bullet","Disable","Stmt","???"] $ \s -> do
        (t,_) <- duration $ evaluate $ length $ take 50 $ BS.findSubstrings (BS.pack s) src
        print (s, t)
        (t,_) <- duration $ evaluate $ length $ BS.findSubstrings (BS.pack s) src
        print (s, t)
    error "done"


-- stage 1

allocIdentifiers :: FilePath -> [Item] -> IO [(Maybe Id, Items)]
allocIdentifiers file xs = withBinaryFile (file <.> "docs") WriteMode $ \h -> do
    forM xs $ \x -> case x of
        Item{..} | Just s <- showItem itemItem -> do
            i <- Id . fromIntegral <$> hTell h
            hPutStrLn h $ show i ++ " " ++ s
            hPutStrLn h itemURL
            hPutStrLn h $ intercalate ", " $ for itemParents $ \xs -> unwords ["<a href=\"" ++ b ++ ">" ++ a ++ "</a>" | (a,b) <- xs]
            hPutStrLn h $ unlines $ replace [""] ["."] $ lines itemDocs
            return $ (Just i, itemItem)
        Item{..} -> return (Nothing, itemItem)
    -- write all the URLs, docs and enough info to pretty print it to a result
    -- and replace each with an identifier (index in the space) - big reduction in memory
    where
        showItem :: Items -> Maybe String
        showItem ITag{} = Nothing
        showItem (IDecl InstDecl{}) = Nothing
        showItem (IDecl x) = Just $ trimStart $ unwords $ words $ prettyPrint $ fmap (const noLoc) x
        showItem (IKeyword x) = Just $ "<b>keyword</b> " ++ x
        showItem (IPackage x) = Just $ "<b>package</b> " ++ x
        showItem (IModule x) = Just $ "<b>module</b> " ++ x


mergeIndentifiers :: [FilePath] -> FilePath -> IO [Id -> Id] -- how to shift each Id
mergeIndentifiers = undefined
    -- find the file size of each file, shift the index by that much
    -- then copy each file into the place

lookupIdentifier :: FilePath -> Int -> IO (URL, Documentation)
lookupIdentifier = undefined



searchNames :: FilePath -> [(Maybe Id, Items)] -> IO ()
searchNames file xs = writeFileBinary (file <.> "words") $ unlines
    [show i ++ " " ++ prettyPrint name | (Just i, IDecl (TypeSig _ [name] _)) <- xs]

searchTypes :: FilePath -> [(Maybe Id, Items)] -> IO ()
searchTypes file xs = writeFileBinary (file <.> "types") $ unlines
    [show i ++ " " ++ trimStart (unwords $ words $ prettyPrint $ fmap (const noLoc) t) | (Just i, IDecl (TypeSig _ _ t)) <- xs]


{-


parse

Package docs ["package:cmdargs","author:Neil Mitchell","license:GPL"] [Module docs "System.Console.CmdArgs" ("docs","cmdargs")]

apply the identifier thing comes along

replace all docs with an identifier, which points at the docs

after the heirarchy comes along lift out the innards

heirarchy :: Package Id -> [Stmt]

then text search is only by statement


-}
