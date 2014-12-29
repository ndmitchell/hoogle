{-# LANGUAGE ViewPatterns, TupleSections #-}
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
--import qualified Data.ByteString.Char8 as BS
--import Control.Exception

import InputHoogle
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

    files <- lines <$> readFile' "input/stackage.txt"
    files <- filterM doesFileExist ["input" </> "hoogle" </> x <.> "txt" | x <- files]
    let n = length files
    forM_ (zip [1..] files) $ \(i,file) -> do
        let out = "output" </> takeBaseName file
        putStr $ "[" ++ show i ++ "/" ++ show n ++ "] " ++ takeBaseName file
        (t,_) <- duration $ do
            src <- readFile' file
            (warns, xs) <- return $ partitionEithers $ parseInputHoogle "" src
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

allocIdentifiers :: FilePath -> [Section (URL, Documentation, Item)] -> IO [Section (Id, Item)]
allocIdentifiers file xs = withBinaryFile (file <.> "ids") WriteMode $ \h -> do
    forM xs $ \x -> case x of
        Section a b -> return $ Section a b
        Item (url,docs,item) -> do
            i <- Id . fromIntegral <$> hTell h
            hPutStrLn h $ show i ++ " " ++ show item
            hPutStrLn h url
            hPutStrLn h docs
            return $ Item (i, item)
    -- write all the URLs, docs and enough info to pretty print it to a result
    -- and replace each with an identifier (index in the space) - big reduction in memory


mergeIndentifiers :: [FilePath] -> FilePath -> IO [Id -> Id] -- how to shift each Id
mergeIndentifiers = undefined
    -- find the file size of each file, shift the index by that much
    -- then copy each file into the place

lookupIdentifier :: FilePath -> Int -> IO (URL, Documentation)
lookupIdentifier = undefined

-- stage 2

flattenHeirarchy :: FilePath -> [Section (Id, Item)] -> IO [(Id, Item)]
flattenHeirarchy file xs = do
    writeFileBinary (file <.> "grp") $ unlines $ f [] (Id 0) xs
    return [x | Item x <- xs]
    where
        f a i (Section k v:xs) =
            [unwords [k, v, show j, show i] | Just j <- [lookup k a]] ++
            f ((k,i):a) i xs
        f a _ (Item (i,_):xs) = f a i xs
        f a _ [] = []
    -- for each identifier write out a grouped file
    -- writes out file like: author Neil (100, 300)

lookupAttribute :: FilePath -> String -> String -> IO [(Int, Int)]
lookupAttribute = undefined

lookupModule :: FilePath -> String -> IO [(Int,Int)]
lookupModule = undefined



searchNames :: FilePath -> [(Id, Item)] -> IO ()
searchNames file xs = writeFileBinary (file <.> "wrd") $ unlines
    [show i ++ " " ++ prettyPrint name | (i, IDecl (TypeSig _ [name] _)) <- xs]


{-


parse

Package docs ["package:cmdargs","author:Neil Mitchell","license:GPL"] [Module docs "System.Console.CmdArgs" ("docs","cmdargs")]

apply the identifier thing comes along

replace all docs with an identifier, which points at the docs

after the heirarchy comes along lift out the innards

heirarchy :: Package Id -> [Stmt]

then text search is only by statement


-}
