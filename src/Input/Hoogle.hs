{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, OverloadedStrings, Rank2Types #-}

module Input.Hoogle(parseHoogle) where

import Language.Haskell.Exts as HSE
import Data.Char
import Data.List.Extra
import Data.Maybe
import Input.Item
import General.Util
import Control.DeepSeq
import Data.IORef.Extra
import System.IO.Unsafe
import Control.Monad.Trans.Class
import qualified Data.Map as Map
import Data.Generics.Uniplate.Data
import General.Conduit
import Control.Monad.Extra
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


-- | Given a file name (for errors), feed in lines to the conduit and emit either errors or items
parseHoogle :: Monad m => (String -> m ()) -> FilePath -> LStr -> Producer m (Target, Item)
parseHoogle warning file body = sourceLStr body =$= linesCR =$= zipFromC 1 =$= parserC warning file =$= hierarchyC hackage =$= mapC (\x -> rnf x `seq` x)

parserC :: Monad m => (String -> m ()) -> FilePath -> Conduit (Int, Str) m (Target, Item)
parserC warning file = f [] ""
    where
        glenum x = IDecl $ TypeSig (SrcLoc "<unknown>.hs" 1 1) [Ident x] (TyCon (UnQual (Ident "GLenum")))

        f com url = do
            x <- await
            whenJust x $ \(i,s) -> case () of
                _ | Just s <- strStripPrefix "-- | " s -> f [s] url
                  | Just s <- strStripPrefix "--" s -> f (if null com then [] else strTrimStart s : com) url
                  | Just s <- strStripPrefix "@url " s -> f com (strUnpack s)
                  | strNull $ strTrimStart s -> f [] ""
                  | Just s <- strStripSuffix " :: GLenum" s -> do
                        -- there are 38K instances of :: GLenum in the OpenGLRaw package, so speed them up (saves 16s + 100Mb)
                        yield (Target url Nothing Nothing $ reformat $ reverse $ map strUnpack com, glenum $ strUnpack s)
                        f [] ""
                  | otherwise -> do
                        case parseLine $ fixLine $ strUnpack s of
                            Left y -> lift $ warning $ file ++ ":" ++ show i ++ ":" ++ y
                            -- only check Nothing as some items (e.g. "instance () :> Foo a")
                            -- don't roundtrip but do come out equivalent
                            Right xs -> forM_ xs $ \x ->
                                if isNothing $ readItem $ showItem x
                                then lift $ warning $ file ++ ":" ++ show i ++ ":failed to roundtrip: " ++ fixLine (strUnpack s)
                                else yield (Target url Nothing Nothing $ reformat $ reverse $ map strUnpack com, descendBi stringShare x)
                        f [] ""


reformat = unlines . replace ["</p>","<p>"] ["</p><p>"] . concatMap f . wordsBy (== "")
    where f xs@(x:_) | x `elem` ["<pre>","<ul>"] = xs
          f xs = ["<p>",unwords xs,"</p>"]


hierarchyC :: Monad m => String -> Conduit (Target, Item) m (Target, Item)
hierarchyC hackage = void $ mapAccumC f (Nothing, Nothing)
    where
        f (pkg, mod) (t, IPackage x) = ((Just (x, url), Nothing), (t{targetURL=url}, IPackage x))
            where url = targetURL t `orIfNull` hackage ++ "package/" ++ x
        f (pkg, mod) (t, IModule x) = ((pkg, Just (x, url)), (t{targetPackage=pkg, targetURL=url}, IModule x))
            where url = targetURL t `orIfNull` maybe "" snd pkg ++ "/docs/" ++ replace "." "-" x ++ ".html" 
        f (pkg, mod) (t, IDecl x) = ((pkg, mod), (t{targetPackage=pkg, targetModule=mod, targetURL=url}, IDecl x))
            where url = targetURL t `orIfNull` maybe "" snd mod ++ "#" ++ declURL x

        orIfNull x y = if null x then y else x

        declURL (TypeSig _ [name] _) = "v:" ++ esc (fromName name)
        declURL x | [x] <- declNames x = "t:" ++ esc x
        declURL x = ""

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
