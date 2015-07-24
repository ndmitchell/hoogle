{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, OverloadedStrings, Rank2Types, DeriveDataTypeable #-}

module Input.Hoogle(parseHoogle, fakePackage, input_hoogle_test) where

import Language.Haskell.Exts as HSE
import Data.Char
import Data.List.Extra
import Data.Data
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

-- | An entry in the Hoogle DB
data Entry = EPackage String
           | EModule String
           | EDecl Decl
             deriving (Data,Typeable,Show)


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

fakePackage :: String -> String -> (Target, Item)
fakePackage name desc = (Target (hackage ++ "package/" ++ name) Nothing Nothing "package" (renderPackage name) desc, IPackage name)

-- | Given a file name (for errors), feed in lines to the conduit and emit either errors or items
parseHoogle :: Monad m => (String -> m ()) -> FilePath -> LStr -> Producer m (Target, Item)
parseHoogle warning file body = sourceLStr body =$= linesCR =$= zipFromC 1 =$= parserC warning file =$= hierarchyC hackage =$= mapC (\x -> rnf x `seq` x)

parserC :: Monad m => (String -> m ()) -> FilePath -> Conduit (Int, Str) m (Target, Entry)
parserC warning file = f [] ""
    where
        glenum x = EDecl $ TypeSig (SrcLoc "<unknown>.hs" 1 1) [Ident x] (TyCon (UnQual (Ident "GLenum")))

        f com url = do
            x <- await
            whenJust x $ \(i,s) -> case () of
                _ | Just s <- strStripPrefix "-- | " s -> f [s] url
                  | Just s <- strStripPrefix "--" s -> f (if null com then [] else strTrimStart s : com) url
                  | Just s <- strStripPrefix "@url " s -> f com (strUnpack s)
                  | strNull $ strTrimStart s -> f [] ""
                  | Just s <- strStripSuffix " :: GLenum" s -> do
                        -- there are 38K instances of :: GLenum in the OpenGLRaw package, so speed them up (saves 16s + 100Mb)
                        yield (Target url Nothing Nothing "" (renderItem $ glenum $ strUnpack s) $ reformat $ reverse $ map strUnpack com, glenum $ strUnpack s)
                        f [] ""
                  | otherwise -> do
                        case parseLine $ fixLine $ strUnpack s of
                            Left y -> lift $ warning $ file ++ ":" ++ show i ++ ":" ++ y
                            -- only check Nothing as some items (e.g. "instance () :> Foo a")
                            -- don't roundtrip but do come out equivalent
                            Right xs -> forM_ xs $ \x ->
                                yield (Target url Nothing Nothing (typeItem x) (renderItem x) $ reformat $ reverse $ map strUnpack com, descendBi stringShare x)
                        f [] ""

typeItem (EPackage x) = "package"
typeItem (EModule x) = "module"
typeItem _ = ""


-- FIXME: used to be in two different modules, now does and then undoes lots of stuff
reformat = trimStart . replace "<p>" "" . replace "</p>" "\n" . unwords . lines .
           unlines . replace ["</p>","<p>"] ["</p><p>"] . concatMap f . wordsBy (== "")
    where f xs@(x:_) | x `elem` ["<pre>","<ul>"] = xs
          f xs = ["<p>",unwords xs,"</p>"]


hierarchyC :: Monad m => String -> Conduit (Target, Entry) m (Target, Item)
hierarchyC hackage = void $ mapAccumC f (Nothing, Nothing)
    where
        f (pkg, mod) (t, EPackage x) = ((Just (x, url), Nothing), (t{targetURL=url}, IPackage x))
            where url = targetURL t `orIfNull` hackage ++ "package/" ++ x
        f (pkg, mod) (t, EModule x) = ((pkg, Just (x, url)), (t{targetPackage=pkg, targetURL=url}, IModule x))
            where url = targetURL t `orIfNull` maybe "" snd pkg ++ "/docs/" ++ replace "." "-" x ++ ".html" 
        f (pkg, mod) (t, EDecl x) = ((pkg, mod), (t{targetPackage=pkg, targetModule=mod, targetURL=url}, IDecl x))
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

renderPackage x = "package <span class=name><0>" ++ escapeHTML x ++ "</0></span>"
renderModule (breakEnd (== '.') -> (pre,post)) = "module " ++ escapeHTML pre ++ "<span class=name><0>" ++ escapeHTML post ++ "</0></span>"


renderItem :: Entry -> String
renderItem = keyword . focus
    where
        keyword x | (a,b) <- word1 x, a `elem` kws = "<b>" ++ dropWhile (== '@') a ++ "</b> " ++ b
                  | otherwise = x
            where kws = words "class data type newtype"

        name x = "<span class=name>" ++ x ++ "</span>" :: String

        focus (EModule x) = renderModule x
        focus (EPackage x) = renderPackage x
        focus (EDecl x) | [now] <- declNames x, (pre,stripPrefix now -> Just post) <- breakOn now $ pretty x =
            if "(" `isSuffixOf` pre && ")" `isPrefixOf` post then
                init (escapeHTML pre) ++ name ("(" ++ highlight now ++ ")") ++ escapeHTML (tail post)
            else
                escapeHTML pre ++ name (highlight now) ++ escapeHTML post
        focus (EDecl x) = pretty x

        highlight :: String -> String
        highlight x = "<0>" ++ escapeHTML x ++ "</0>"


parseLine :: String -> Either String [Entry]
parseLine x@('@':str) = case a of
        "package" | [b] <- words b, b /= "" -> Right [EPackage b]
        "version" -> Right []
        _ -> Left $ "unknown attribute: " ++ x
    where (a,b) = word1 str
parseLine (stripPrefix "module " -> Just x) = Right [EModule x]
parseLine x | Just x <- readItem x = case x of
    TypeSig a bs c -> Right [EDecl (TypeSig a [b] c) | b <- bs]
    x -> Right [EDecl x]
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


readItem :: String -> Maybe Decl
readItem x | ParseOk y <- myParseDecl x = Just $ unGADT y
readItem x -- newtype
    | Just x <- stripPrefix "newtype " x
    , ParseOk (DataDecl a _ c d e f g) <- fmap unGADT $ myParseDecl $ "data " ++ x
    = Just $ DataDecl a NewType c d e f g
readItem x -- constructors
    | ParseOk (GDataDecl _ _ _ _ _ _ [GadtDecl s name _ ty] _) <- myParseDecl $ "data Data where " ++ x
    = Just $ TypeSig s [name] ty
readItem ('(':xs) -- tuple constructors
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (TypeSig s [Ident _] ty) <- myParseDecl $ replicate (length com + 2) 'a' ++ rest
    = Just $ TypeSig s [Ident $ '(':com++")"] ty
readItem (stripPrefix "data (" -> Just xs)  -- tuple data type
    | (com,')':rest) <- span (== ',') xs
    , ParseOk (DataDecl a b c _ e f g) <- fmap unGADT $ myParseDecl $
        "data " ++ replicate (length com + 2) 'A' ++ rest
    = Just $ DataDecl a b c (Ident $ '(':com++")") e f g
readItem _ = Nothing

myParseDecl = parseDeclWithMode parseMode -- partial application, to share the initialisation cost

unGADT (GDataDecl a b c d e _ [] f) = DataDecl a b c d e [] f
unGADT x = x

prettyItem :: Entry -> String
prettyItem (EPackage x) = "package " ++ x
prettyItem (EModule x) = "module " ++ x
prettyItem (EDecl x) = pretty x


input_hoogle_test :: IO ()
input_hoogle_test = testing "Input.Hoogle.parseLine" $ do
    let a === b | fmap (map prettyItem) (parseLine a) == Right [b] = putChar '.'
                | otherwise = error $ show (a,b,parseLine a, fmap (map prettyItem) $ parseLine a)
    let test a = a === a
    test "type FilePath = [Char]"
    test "data Maybe a"
    test "Nothing :: Maybe a"
    test "Just :: a -> Maybe a"
    test "newtype Identity a"
    test "foo :: Int# -> b"
    test "(,,) :: a -> b -> c -> (a, b, c)"
    test "data (,,) a b"
    test "reverse :: [a] -> [a]"
    test "reverse :: [:a:] -> [:a:]"
    test "module Foo.Bar"
    test "data Char"
    "data Char :: *" === "data Char"
    "newtype ModuleName :: *" === "newtype ModuleName"
    -- Broken in the last HSE release, fixed in HSE HEAD
    -- test "quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)"
    test "( # ) :: Int"
