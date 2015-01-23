{-# LANGUAGE ViewPatterns, PatternGuards #-}

module Input.Type(
    Database(..),
    ItemEx(..), Item(..),
    showItem, prettyItem, readItem,
    isIPackage, isIModule, splitIPackage, splitIModule,
    URL, Documentation,
    Id(..),
    test
    ) where

import Numeric
import Data.Tuple.Extra
import Language.Haskell.Exts
import Data.List
import General.Util
import Data.List.Extra
import Data.Maybe

newtype Database = Database FilePath

---------------------------------------------------------------------
-- DATABASE

type URL = String
type Documentation = String
newtype Id = Id Int deriving (Eq,Ord)

instance Show Id where
    show (Id x) = showHex x ""

instance Read Id where
    readsPrec _ = map (first Id) . readHex

data ItemEx = ItemEx
    {itemItem :: Item
    ,itemURL :: URL
    ,itemPackage :: Maybe (String, URL)
    ,itemModule :: Maybe (String, URL)
    ,itemDocs :: Documentation
    } deriving (Show,Eq)

data Item
    = IDecl {fromIDecl :: Decl}
    | IKeyword {fromIKeyword :: String}
    | IPackage {fromIPackage :: String}
    | IModule {fromIModule :: String}
      deriving (Show,Eq)

isIModule IModule{} = True; isIModule _ = False
isIPackage IPackage{} = True; isIPackage _ = False

splitIPackage, splitIModule :: [(a, Item)] -> [(String, [(a, Item)])]
splitIPackage = splitUsing $ \x -> case snd x of IPackage x -> Just x; _ -> Nothing
splitIModule = splitUsing $ \x -> case snd x of IModule x -> Just x; _ -> Nothing

splitUsing :: (a -> Maybe String) -> [a] -> [(String, [a])]
splitUsing f = repeatedly $ \(x:xs) ->
    let (a,b) = break (isJust . f) xs
    in ((fromMaybe "" $ f x, x:a), b)


---------------------------------------------------------------------
-- ITEM AS STRING

showItem :: Item -> String
showItem (IKeyword x) = "@keyword " ++ x
showItem (IPackage x) = "@package " ++ x
showItem (IModule x) = "module " ++ x
showItem (IDecl x) = pretty x

prettyItem :: Item -> String
prettyItem (IKeyword x) = "keyword " ++ x
prettyItem (IPackage x) = "package " ++ x
prettyItem x = showItem x

readItem :: String -> Maybe Item
readItem (stripPrefix "@keyword " -> Just x) = Just $ IKeyword x
readItem (stripPrefix "@package " -> Just x) = Just $ IPackage x
readItem (stripPrefix "module " -> Just x) = Just $ IModule x
readItem x | ParseOk y <- myParseDecl x = Just $ IDecl $ unGADT y
readItem x -- newtype
    | Just x <- stripPrefix "newtype " x
    , ParseOk (DataDecl a _ c d e f g) <- fmap unGADT $ myParseDecl $ "data " ++ x
    = Just $ IDecl $ DataDecl a NewType c d e f g
readItem x -- constructors
    | ParseOk (GDataDecl _ _ _ _ _ _ [GadtDecl s name _ ty] _) <- myParseDecl $ "data Data where " ++ x
    = Just $ IDecl $ TypeSig s [name] ty
readItem o@('(':xs) -- tuple definitions
    | ")" `isPrefixOf` rest
    , ParseOk y <- myParseDecl $ replicate (length com + 2) 'a' ++ drop 1 rest
    = Just $ IDecl $ f y
    where
        (com,rest) = span (== ',') xs
        f (TypeSig s [Ident _] ty) = TypeSig s [Ident $ '(':com++")"] ty
readItem _ = Nothing

myParseDecl = parseDeclWithMode parseMode -- partial application, to share the initialisation cost

unGADT (GDataDecl a b c d e _ [] f) = DataDecl a b c d e [] f
unGADT x = x


test :: IO ()
test = testing "Input.Type.readItem" $ do
    let a === b | fmap prettyItem (readItem a) == Just b = putChar '.'
                | otherwise = error $ show (a,b,readItem a, fmap prettyItem $ readItem a)
    let test a = a === a
    test "type FilePath = [Char]"
    test "data Maybe a"
    test "Nothing :: Maybe a"
    test "Just :: a -> Maybe a"
    test "newtype Identity a"
    test "foo :: Int# -> b"
    test "(,,) :: a -> b -> c -> (a, b, c)"
    test "reverse :: [a] -> [a]"
    test "reverse :: [:a:] -> [:a:]"
    test "module Foo.Bar"
    test "data Char"
    "data Char :: *" === "data Char"
    "newtype ModuleName :: *" === "newtype ModuleName"
    -- Broken in the last HSE release, fixed in HSE HEAD
    -- test "quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)"
    test "( # ) :: Int"
