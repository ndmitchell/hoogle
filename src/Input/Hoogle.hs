{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Hoogle(parseHoogle) where

import Language.Haskell.Exts as HSE
import Data.Char
import Data.List.Extra
import Data.Maybe
import Input.Type
import General.Util
import Control.DeepSeq
import Data.IORef.Extra
import System.IO.Unsafe
import qualified Data.Map as Map
import Data.Generics.Uniplate.Data


hackage = "https://hackage.haskell.org/"


{-# NOINLINE strings #-}
strings :: IORef (Map.Map Name Name)
strings = unsafePerformIO $ newIORef Map.empty

-- Increases creation time from 27s to 28s
-- Reduces peak memory from 767Mb to 625Mb, and maximum resident with profiling from 100Mb to 45Mb
-- Using Name over String is about 20% better
stringShare :: Name -> Name
stringShare x = unsafePerformIO $ do
    mp <- readIORef strings
    case Map.lookup x mp of
        Just x -> return x
        Nothing -> do
            writeIORef' strings $ Map.insert x x mp
            return x


-- | Given a Hoogle database, grab the Item (Right), or things I failed to parse (Left)
parseHoogle :: FilePath -> String -> [Either String ItemEx]
{-
parseHoogle = f [] . lines
    where
        f com ((stripPrefix "-- " -> Just x):xs) = f (com ++ [x]) xs
        f com (x:xs) | all isSpace x = f [] xs
        f com (('@': (word1 -> (key,val))):xs) = Right (Tagged key val) : f [] xs
        f com ((stripPrefix "module " -> Just x):xs) = Right (Tagged "module" x) : f [] xs
        f com (x:xs) | ParseOk res <- parseDecl x = Right (ItemEx $ ItemEx "http:" (unlines com) [] $ IDecl $ fmap (const ()) res) : f [] xs
        f com (x:xs) = Left ("Could not parse line: " ++ x) : f [] xs
        f com [] = []


parseInputHaskell :: HackageURL -> String -> ([ParseError], Input)
parseInputHaskell hackage =
-}
parseHoogle file = heirarchy hackage . f [] "" . zip [1..] . lines
    where
        f :: [String] -> URL -> [(Int,String)] -> [Either String ItemEx]
        f com url [] = []
        f com url ((i,s):is)
            | "-- | " `isPrefixOf` s = f [drop 5 s] url is
            | "--" `isPrefixOf` s = f ([dropWhile isSpace $ drop 2 s | com /= []] ++ com) url is
            | "@url " `isPrefixOf` s =  f com (drop 5 s) is
            | all isSpace s = f [] "" is
            | otherwise = (case parseLine $ fixLine s of
                               Left y -> [Left $ file ++ ":" ++ show i ++ ":" ++ y | not $ "@version " `isPrefixOf` s]
                               -- only check Nothing as some items (e.g. "instance () :> Foo a")
                               -- don't roundtrip but do come out equivalent
                               Right xs | any (isNothing . readItem . showItem) xs ->
                                       [Left $ file ++ ":" ++ show i ++ ":failed to roundtrip: " ++ fixLine s]
                               Right xs -> [Right $ ItemEx (descendBi stringShare x) url Nothing Nothing (reformat $ reverse com) | x <- xs]
                          )
                          ++ f [] "" is

reformat = unlines . replace ["</p>","<p>"] ["</p><p>"] . concatMap f . wordsBy (== "")
    where f xs@(x:_) | x `elem` ["<pre>","<ul>"] = xs
          f xs = ["<p>",unwords xs,"</p>"]


heirarchy :: NFData a => URL -> [Either a ItemEx] -> [Either a ItemEx]
heirarchy hackage = list' . map other . with (isIModule . itemItem) . map modules . with (isIPackage . itemItem) . map packages
    where
        with :: (b -> Bool) -> [Either a b] -> [Either a (Maybe b, b)]
        with p = snd . mapAccumL f Nothing
            where
                f s (Left e) = (s,Left e)
                f s (Right x) = let s2 = if p x then Just x else s in (s2,Right (s2,x))

        packages (Right i@ItemEx{itemItem=IPackage x, itemURL=""}) = Right i{itemURL = hackage ++ "package/" ++ x}
        packages i = i

        modules (Right (Just ItemEx{itemItem=IPackage pname, itemURL=purl}, i@ItemEx{itemItem=IModule x})) = Right i
            {itemURL = if null $ itemURL i then purl ++ "/docs/" ++ replace "." "-" x ++ ".html" else itemURL i
            ,itemPackage = Just (pname, purl)}
        modules (Right (_, i)) = Right i
        modules (Left x) = Left x

        other (Right (Just ItemEx{itemItem=IModule mname, itemURL=murl, itemPackage=pkg}, i@ItemEx{itemItem=IDecl x})) = Right i
            {itemURL = if null $ itemURL i then murl ++ "#" ++ url x else itemURL i
            ,itemPackage = pkg
            ,itemModule = Just (mname, murl)}
        other (Right (_, i)) = Right i
        other (Left x) = Left x

        url (TypeSig _ [name] _) = "v:" ++ esc (fromName name)
        url x | [x] <- declNames x = "t:" ++ esc x
        url x = ""

        esc = concatMap f
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


parseLine :: String -> Either String [Item]
parseLine x@('@':str) = case a of
        "keyword" | b <- words b, b /= [] -> Right [IKeyword $ unwords b]
        "package" | [b] <- words b, b /= "" -> Right [IPackage b]
        _ -> Left $ "unknown attribute: " ++ x
    where (a,b) = word1 str
parseLine x | Just x <- readItem x = case x of
    IDecl (TypeSig a bs c) -> Right [IDecl (TypeSig a [b] c) | b <- bs]
    x -> Right [x]
parseLine x = Left $ "failed to parse: " ++ x


fixLine :: String -> String
fixLine (stripPrefix "instance [incoherent] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlap ok] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [safe] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "(#) " -> Just x) = "( # ) " ++ x
fixLine ('[':x:xs) | isAlpha x || x == '_', (a,']':b) <- break (== ']') xs = x : a ++ b
fixLine ('[':':':xs) | (a,']':b) <- break (== ']') xs = "(:" ++ a ++ ")" ++ b
fixLine x | "class " `isPrefixOf` x = fst $ breakOn " where " x
fixLine x = x
