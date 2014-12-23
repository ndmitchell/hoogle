{-# LANGUAGE ViewPatterns, TupleSections #-}

-- grp = 1.28Mb
-- wrd = 10.7Mb

-- [(".grp",1_343_808),(".ids",247_336_269),(".wrd",11_230_952)]
-- [(".grp",1_314_256),(".ids",244_154_208),(".wrd",7_369_220)]


import Language.Haskell.Exts.Annotated
import Data.Char
import Control.Applicative
import System.IO.Extra
import Data.List.Extra
import System.FilePath
import Control.Monad
import System.Directory.Extra
import System.Time.Extra
import Data.Tuple.Extra
import Numeric
import qualified Data.ByteString.Char8 as BS
import Control.Exception


data Block = StmtEx StmtEx
           | Attrib String String
           | BlockModule String
             deriving Show

data StmtEx = Stmt (Decl ())
            | Keyword String
            | StmtPackage String
            | StmtModule String
              deriving Show

type URL = String
type Documentation = String
type Id = Int

main :: IO ()
main = do
    src <- BS.readFile "output/bullet.ids"
    forM_ ["Bullet","Disable","Stmt","???"] $ \s -> do
        (t,_) <- duration $ evaluate $ length $ take 50 $ BS.findSubstrings (BS.pack s) src
        print (s, t)
        (t,_) <- duration $ evaluate $ length $ BS.findSubstrings (BS.pack s) src
        print (s, t)
    error "done"


    files <- listFiles "hoogle"
    let n = length files
    forM_ (zip [1..] files) $ \(i,file) -> do
        let out = "output" </> takeBaseName file
        putStr $ "[" ++ show i ++ "/" ++ show n ++ "] " ++ takeBaseName file
        (t,_) <- duration $ do
            xs <- parseDocsFile file
            xs <- allocIdentifiers out xs
            xs <- flattenHeirarchy out xs
            searchNames out xs
        putStrLn $ " in " ++ show (round t) ++ "s"
    files <- listFiles "output"
    files <- forM files $ \file -> (takeExtension file,) <$> fileSize file
    print $ map (second sum) $ groupSort files
    print "done"

-- stage 0

parseDocsFile :: FilePath -> IO [(Block, Maybe (URL, Documentation))]
parseDocsFile file = f [] . lines <$> readFile' file
    where
        f :: [String] -> [String] -> [(Block, Maybe (URL, Documentation))]
        f com ((stripPrefix "-- " -> Just x):xs) = f (com ++ [x]) xs
        f com (x:xs) | all isSpace x = f [] xs
        f com (('@': (word1 -> (key,val))):xs) = (Attrib key val, Nothing) : f [] xs
        f com ((stripPrefix "module " -> Just x):xs) = (BlockModule x, Nothing) : f [] xs
        f com (x:xs) | ParseOk res <- parseDecl x = (StmtEx $ Stmt $ fmap (const ()) res, Just ("http:",unlines com)) : f [] xs
        f com (x:xs) = f [] xs -- error $ "Could not parse line: " ++ show x
        f com [] = []


-- stage 1

allocIdentifiers :: FilePath -> [(Block, Maybe (URL, Documentation))] -> IO [(Block, Maybe Id)]
allocIdentifiers file xs = withBinaryFile (file <.> "ids") WriteMode $ \h -> do
    forM xs $ \(a,b) -> case b of
        Nothing -> return (a,Nothing)
        Just (url,docs) -> do
            i <- fromIntegral <$> hTell h
            hPutStrLn h $ idHex i ++ " " ++ show a
            hPutStrLn h url
            hPutStrLn h docs
            return (a,Just i)
    -- write all the URLs, docs and enough info to pretty print it to a result
    -- and replace each with an identifier (index in the space) - big reduction in memory


mergeIndentifiers :: [FilePath] -> FilePath -> IO [Id -> Id] -- how to shift each Id
mergeIndentifiers = undefined
    -- find the file size of each file, shift the index by that much
    -- then copy each file into the place

lookupIdentifier :: FilePath -> Int -> IO (URL, Documentation)
lookupIdentifier = undefined

-- stage 2

flattenHeirarchy :: FilePath -> [(Block, Maybe Id)] -> IO [(StmtEx, Id)]
flattenHeirarchy file xs = do
    writeFileBinary (file <.> "grp") $ unlines $ f [] 0 xs
    return [(a,b) | (StmtEx a,Just b) <- xs]
    where
        f a i ((BlockModule v, b):xs) = f a i $ (Attrib "module" v, b):xs
        f a i ((Attrib k v,_):xs) =
            [unwords [k, v, idHex j, idHex i] | Just j <- [lookup k a]] ++
            f ((k,i):a) i xs
        f a _ ((s, Just i):xs) = f a i xs
        f a _ [] = []
    -- for each identifier write out a grouped file
    -- writes out file like: author Neil (100, 300)

lookupAttribute :: FilePath -> String -> String -> IO [(Int, Int)]
lookupAttribute = undefined

lookupModule :: FilePath -> String -> IO [(Int,Int)]
lookupModule = undefined



searchNames :: FilePath -> [(StmtEx, Id)] -> IO ()
searchNames file xs = writeFileBinary (file <.> "wrd") $ unlines
    [idHex i ++ " " ++ prettyPrint name | (Stmt (TypeSig _ [name] _), i) <- xs]


{-


parse

Package docs ["package:cmdargs","author:Neil Mitchell","license:GPL"] [Module docs "System.Console.CmdArgs" ("docs","cmdargs")]

apply the identifier thing comes along

replace all docs with an identifier, which points at the docs

after the heirarchy comes along lift out the innards

heirarchy :: Package Id -> [Stmt]

then text search is only by statement


-}

fileSize :: FilePath -> IO Int
fileSize file = withFile file ReadMode $ fmap fromIntegral . hFileSize

idHex :: Id -> String
idHex i = showHex i ""
