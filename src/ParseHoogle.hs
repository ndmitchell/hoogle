{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module ParseHoogle(parseHoogle) where

import Language.Haskell.Exts.Annotated as HSE
import Data.Char
import Data.List.Extra
import Data.Either
import Data.Tuple.Extra
import Type


hackage = "https://hackage.haskell.org/"


parseHoogle :: String -> [Either String (Tagged ItemEx)]
{-
parseHoogle = f [] . lines
    where
        f com ((stripPrefix "-- " -> Just x):xs) = f (com ++ [x]) xs
        f com (x:xs) | all isSpace x = f [] xs
        f com (('@': (word1 -> (key,val))):xs) = Right (Tagged key val) : f [] xs
        f com ((stripPrefix "module " -> Just x):xs) = Right (Tagged "module" x) : f [] xs
        f com (x:xs) | ParseOk res <- parseDecl x = Right (Item $ ItemEx "http:" (unlines com) [] $ IDecl $ fmap (const ()) res) : f [] xs
        f com (x:xs) = Left ("Could not parse line: " ++ x) : f [] xs
        f com [] = []


parseInputHaskell :: HackageURL -> String -> ([ParseError], Input)
parseInputHaskell hackage =
-}
parseHoogle = unpartitionEithers . second (assignURLs hackage) . partitionEithers . f [] "" . zip [1..] . lines
    where
        f :: [String] -> URL -> [(Int,String)] -> [Either String (Tagged ItemEx)]
        f com url [] = []
        f com url ((i,s):is)
            | "-- | " `isPrefixOf` s = f [drop 5 s] url is
            | "--" `isPrefixOf` s = f ([dropWhile isSpace $ drop 2 s | com /= []] ++ com) url is
            | "@url " `isPrefixOf` s =  f com (drop 5 s) is
            | all isSpace s = f [] "" is
            | otherwise = (case parseLine i s of
                               Left y -> [Left y | not $ "@version " `isPrefixOf` s]
                               Right x -> [Right $ Item $ ItemEx url (unlines $ reverse com) [] x]
                          )
                          ++ f [] "" is

unpartitionEithers (as,bs) = map Left as ++ map Right bs

-- Given a URL for hackage, and a list of items, give each thing a URL
assignURLs :: URL -> [Tagged ItemEx] -> [Tagged ItemEx]
assignURLs hackage = f "" ""
    where
        f pkg mod (Item i:xs) = Item i{itemURL=hackage ++ "???"} : f pkg mod xs
        f pkg mod (x:xs) = x : f pkg mod xs
        f pkg mod [] = []
{-
    esc = concatMap f
        where
            f x | isAlphaNum x = [x]
                | otherwise = "-" ++ show (ord x) ++ "-"
-}


parseLine :: Int -> String -> Either String Item
parseLine line x | "(##)" `isPrefixOf` x = Left $ show line ++ ": skipping due to HSE bug #206 on (##)"
parseLine line ('@':str) = case a of
        "keyword" | b <- words b, b /= [] -> Right $ IKeyword $ unwords b
        "package" | [b] <- words b, b /= "" -> Right $ IPackage b
        _ -> Left $ show line ++ ": unknown attribute, " ++ a
    where (a,b) = break isSpace str
parseLine line x | ["module",a] <- words x = Right $ IModule a

-- normal decls
parseLine line x
    | ParseOk y <- parseDeclWithMode defaultParseMode{extensions=exts} $ x ++ ex
    = Right $ transDecl y
    where ex = if "newtype " `isPrefixOf` x then " = N T" else " " -- space to work around HSE bug #205

-- constructors
parseLine line x
    | ParseOk y <- parseDeclWithMode defaultParseMode{extensions=exts} $ "data Data where " ++ x
    = Right $ transDecl $ fmap (subtractCols 16) y
    where
        subtractCols :: Int -> SrcSpanInfo -> SrcSpanInfo
        subtractCols n (SrcSpanInfo x xs) = SrcSpanInfo (f x) (map f xs)
            where f x = x{srcSpanStartColumn=srcSpanStartColumn x - n, srcSpanEndColumn=srcSpanEndColumn x - n}

-- tuple definitions
parseLine line o@('(':xs)
    | ")" `isPrefixOf` rest
    , ParseOk y <- parseDeclWithMode defaultParseMode{extensions=exts} $ replicate (length com + 2) 'a' ++ drop 1 rest
    = Right $ transDecl $ f y
    where
        (com,rest) = span (== ',') xs
        f (HSE.TypeSig sl [Ident sl2 _] ty) = HSE.TypeSig sl [Ident sl2 $ '(':com++")"] ty

parseLine line x = Left $ show line ++ ":failed to parse: " ++ x

exts = map EnableExtension
    [EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses
    ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples]


---------------------------------------------------------------------
-- TRANSLATE THINGS

type S = SrcSpanInfo

transDecl :: Decl S -> Item
transDecl (GDataDecl s dat ctxt hd _ [] _) = transDecl $ DataDecl s dat ctxt hd [] Nothing
transDecl (GDataDecl _ _ _ _ _ [GadtDecl s name _ ty] _) = transDecl $ HSE.TypeSig s [name] ty
transDecl x = IDecl $ fmap (const ()) x
