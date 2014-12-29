{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards #-}
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
import Data.Maybe
--import qualified Data.ByteString.Char8 as BS
--import Control.Exception

import ParseHoogle
import Type
import Util

main :: IO ()
main = do
    {-
    src <- BS.readFile "output/bullet.ids"
    forM_ ["Bullet","Disable","Stmt","???"] $ \s -> do
        (t,_) <- duration $ evaluate $ length $ take 50 $ BS.findSubstrings (BS.pack s) src
        print (s, t)
        (t,_) <- duration $ evaluate $ length $ BS.findSubstrings (BS.pack s) src
        print (s, t)
    error "done"
    -}

    args <- getArgs
    files <- if args /= [] then return ["input/hoogle" </> x <.> "txt" | x <- args] else do
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
            xs <- flattenHeirarchy out xs
            searchNames out xs
        putStrLn $ " in " ++ show (round t) ++ "s"
    files <- listFiles "output"
    files <- forM files $ \file -> (takeExtension file,) <$> fileSize file
    print $ map (second sum) $ groupSort files
    print "done"

-- stage 1

allocIdentifiers :: FilePath -> [Tagged ItemEx] -> IO [Tagged (Maybe Id, Item)]
allocIdentifiers file xs = withBinaryFile (file <.> "docs") WriteMode $ \h -> do
    forM xs $ \x -> case x of
        Tagged a b -> return $ Tagged a b
        Item ItemEx{..} | Just s <- showItem itemItem -> do
            i <- Id . fromIntegral <$> hTell h
            hPutStrLn h $ show i ++ " " ++ s
            hPutStrLn h itemURL
            hPutStrLn h $ unlines $ replace [""] ["."] $ lines itemDocs
            return $ Item (Just i, itemItem)
        Item i -> return $ Item (Nothing, itemItem i)
    -- write all the URLs, docs and enough info to pretty print it to a result
    -- and replace each with an identifier (index in the space) - big reduction in memory
    where
        showItem :: Item -> Maybe String
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

-- stage 2

flattenHeirarchy :: FilePath -> [Tagged (Maybe Id, Item)] -> IO [(Maybe Id, Item)]
flattenHeirarchy file xs = do
    writeFileBinary (file <.> "groups") $ unlines $ f [] (Id 0) xs
    return [x | Item x <- xs]
    where
        f a i (Tagged k v:xs) =
            [unwords [k, v, show j, show i] | Just j <- [lookup k a]] ++
            f ((k,i):a) i xs
        f a j (Item (i,_):xs) = f a (fromMaybe j i) xs
        f a _ [] = []
    -- for each identifier write out a grouped file
    -- writes out file like: author Neil (100, 300)

lookupAttribute :: FilePath -> String -> String -> IO [(Int, Int)]
lookupAttribute = undefined

lookupModule :: FilePath -> String -> IO [(Int,Int)]
lookupModule = undefined



searchNames :: FilePath -> [(Maybe Id, Item)] -> IO ()
searchNames file xs = writeFileBinary (file <.> "words") $ unlines
    [show i ++ " " ++ prettyPrint name | (Just i, IDecl (TypeSig _ [name] _)) <- xs]


{-


parse

Package docs ["package:cmdargs","author:Neil Mitchell","license:GPL"] [Module docs "System.Console.CmdArgs" ("docs","cmdargs")]

apply the identifier thing comes along

replace all docs with an identifier, which points at the docs

after the heirarchy comes along lift out the innards

heirarchy :: Package Id -> [Stmt]

then text search is only by statement


-}
