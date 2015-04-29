{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, OverloadedStrings #-}

module Input.Hoogle(parseHoogle, parseHoogleC) where

import Language.Haskell.Exts as HSE
import Data.Char
import Data.List.Extra
import Data.Maybe
import Input.Type
import General.Util
import Data.IORef.Extra
import System.IO.Unsafe
import qualified Data.Map as Map
import Data.Generics.Uniplate.Data
import General.Conduit
import Control.Monad.Extra
import Data.Functor.Identity
import General.Str


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
parseHoogle :: FilePath -> LStr -> [Either String ItemEx]
parseHoogle file body = list' $ runIdentity $ runConduit $ sourceLStr body |> linesCR |> parseHoogleC file |> sinkList

-- | Given a file name (for errors), feed in lines to the conduit and emit either errors or items
parseHoogleC :: Monad m => FilePath -> Conduit Str m (Either String ItemEx)
parseHoogleC file = zipFromC 1 |> parserC file |> rightsC (hierarchyC hackage)

parserC :: Monad m => FilePath -> Conduit (Int, Str) m (Either String ItemEx)
parserC file = f [] ""
    where
        f com url = do
            x <- await
            whenJust x $ \(i,s) -> case () of
                _ | Just s <- strStripPrefix "-- | " s -> f [s] url
                  | Just s <- strStripPrefix "--" s -> f (if null com then [] else strTrimStart s : com) url
                  | Just s <- strStripPrefix "@url " s -> f com (strUnpack s)
                  | strNull $ strTrimStart s -> f [] ""
                  | otherwise -> do
                        case parseLine $ fixLine $ strUnpack s of
                            Left y -> yield $ Left $ file ++ ":" ++ show i ++ ":" ++ y
                            -- only check Nothing as some items (e.g. "instance () :> Foo a")
                            -- don't roundtrip but do come out equivalent
                            Right xs -> forM_ xs $ \x -> yield $
                                if isNothing $ readItem $ showItem x
                                then Left $ file ++ ":" ++ show i ++ ":failed to roundtrip: " ++ fixLine (strUnpack s)
                                else Right $ ItemEx (descendBi stringShare x) url Nothing Nothing (reformat $ reverse $ map strUnpack com)
                        f [] ""


reformat = unlines . replace ["</p>","<p>"] ["</p><p>"] . concatMap f . wordsBy (== "")
    where f xs@(x:_) | x `elem` ["<pre>","<ul>"] = xs
          f xs = ["<p>",unwords xs,"</p>"]


hierarchyC :: Monad m => String -> Conduit ItemEx m ItemEx
hierarchyC hackage = mapC packages |> with (isIPackage . itemItem) |> mapC modules |> with (isIModule . itemItem) |> mapC other
    where
        with :: Monad m => (b -> Bool) -> Conduit b m (Maybe b, b)
        with p = void $ mapAccumC f Nothing
            where f s x = let s2 = if p x then Just x else s in (s2,(s2,x))

        packages i@ItemEx{itemItem=IPackage x, itemURL=""} = i{itemURL = hackage ++ "package/" ++ x}
        packages i = i

        modules (Just ItemEx{itemItem=IPackage pname, itemURL=purl}, i@ItemEx{itemItem=IModule x}) = i
            {itemURL = if null $ itemURL i then purl ++ "/docs/" ++ replace "." "-" x ++ ".html" else itemURL i
            ,itemPackage = Just (pname, purl)}
        modules (_, i) = i

        other (Just ItemEx{itemItem=IModule mname, itemURL=murl, itemPackage=pkg}, i@ItemEx{itemItem=IDecl x}) = i
            {itemURL = if null $ itemURL i then murl ++ "#" ++ url x else itemURL i
            ,itemPackage = pkg
            ,itemModule = Just (mname, murl)}
        other (_, i) = i

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
        "version" -> Right []
        _ -> Left $ "unknown attribute: " ++ x
    where (a,b) = word1 str
parseLine x | Just x <- readItem x = case x of
    IDecl (TypeSig a bs c) -> Right [IDecl (TypeSig a [b] c) | b <- bs]
    x -> Right [x]
parseLine x = Left $ "failed to parse: " ++ x


fixLine :: String -> String
fixLine (stripPrefix "instance [incoherent] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlap ok] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlapping] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [safe] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "(#) " -> Just x) = "( # ) " ++ x
fixLine ('[':x:xs) | isAlpha x || x `elem` ("_(" :: String), (a,']':b) <- break (== ']') xs = x : a ++ b
fixLine ('[':':':xs) | (a,']':b) <- break (== ']') xs = "(:" ++ a ++ ")" ++ b
fixLine x | "class " `isPrefixOf` x = fst $ breakOn " where " x
fixLine "(+, -, *) :: Num a => a -> a -> a" = "(+), (-), (*) :: Num a => a -> a -> a"
fixLine "(**, logBase) :: Floating a => a -> a -> a" = "(**), logBase :: Floating a => a -> a -> a"
fixLine x = x
