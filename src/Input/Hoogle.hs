{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections #-}

module Input.Hoogle(parseHoogle) where

import Language.Haskell.Exts as HSE
import Data.Char
import Data.List.Extra
import Input.Type


hackage = "https://hackage.haskell.org/"


-- | Given a Hoogle database, grab the items (Right), or things I failed to parse (Left)
parseHoogle :: String -> [Either String Item]
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
parseHoogle = heirarchy hackage . f [] "" . zip [1..] . lines
    where
        f :: [String] -> URL -> [(Int,String)] -> [Either String Item]
        f com url [] = []
        f com url ((i,s):is)
            | "-- | " `isPrefixOf` s = f [drop 5 s] url is
            | "--" `isPrefixOf` s = f ([dropWhile isSpace $ drop 2 s | com /= []] ++ com) url is
            | "@url " `isPrefixOf` s =  f com (drop 5 s) is
            | all isSpace s = f [] "" is
            | otherwise = (case parseLine i s of
                               Left y -> [Left y | not $ "@version " `isPrefixOf` s]
                               Right x -> [Right $ Item url (reformat $ reverse com) [] x]
                          )
                          ++ f [] "" is

reformat = unlines . replace ["</p>","<p>"] ["</p><p>"] . concatMap f . wordsBy (== "")
    where f xs@(x:_) | x `elem` ["<pre>","<ul>"] = xs
          f xs = ["<p>",unwords xs,"</p>"]


heirarchy :: URL -> [Either a Item] -> [Either a Item]
heirarchy hackage = map other . with (isIModule . itemItem) . map modules . with (isIPackage . itemItem) . map packages
    where
        with :: (b -> Bool) -> [Either a b] -> [Either a (Maybe b, b)]
        with p = snd . mapAccumL f Nothing
            where
                f s (Left e) = (s,Left e)
                f s (Right x) = let s2 = if p x then Just x else s in (s2,Right (s2,x))

        packages (Right i@Item{itemItem=IPackage x, itemURL=""}) = Right i{itemURL = hackage ++ "package/" ++ x}
        packages i = i

        modules (Right (Just Item{itemItem=IPackage pname, itemURL=purl}, i@Item{itemItem=IModule x})) = Right i
            {itemURL = if null $ itemURL i then purl ++ "/docs/" ++ replace "." "-" x ++ ".html" else itemURL i
            ,itemParents = [[(pname, purl)]]}
        modules (Right (_, i)) = Right i
        modules (Left x) = Left x

        other (Right (Just Item{itemItem=IModule mname, itemURL=murl, itemParents=mpar}, i@Item{itemItem=IDecl x})) = Right i
            {itemURL = if null $ itemURL i then murl ++ "#" ++ url x else itemURL i
            ,itemParents = map (++ [(mname, murl)]) mpar}
        other (Right (_, i)) = Right i
        other (Left x) = Left x

        url (TypeSig _ [name] _) = "v:" ++ esc (prettyPrint name)
        url _ = ""

        esc = concatMap f
            where
                f x | isAlphaNum x = [x]
                    | otherwise = "-" ++ show (ord x) ++ "-"


parseLine :: Int -> String -> Either String Items
parseLine line x | "(##)" `isPrefixOf` x = Left $ show line ++ ": skipping due to HSE bug #206 on (##)"
parseLine line ('@':str) = case a of
        "keyword" | b <- words b, b /= [] -> Right $ IKeyword $ unwords b
        "package" | [b] <- words b, b /= "" -> Right $ IPackage b
        _ -> Left $ show line ++ ": unknown attribute, " ++ a
    where (a,b) = word1 str
parseLine line x | ["module",a] <- words x = Right $ IModule a

-- normal decls
parseLine line x
    | ParseOk y <- parseDeclWithMode defaultParseMode{extensions=exts} $ x ++ ex
    = Right $ transDecl y
    where ex = if "newtype " `isPrefixOf` x then " = N T" else " " -- space to work around HSE bug #205

-- constructors
parseLine line x
    | ParseOk y <- parseDeclWithMode defaultParseMode{extensions=exts} $ "data Data where " ++ x
    = Right $ transDecl y

-- tuple definitions
parseLine line o@('(':xs)
    | ")" `isPrefixOf` rest
    , ParseOk y <- parseDeclWithMode defaultParseMode{extensions=exts} $ replicate (length com + 2) 'a' ++ drop 1 rest
    = Right $ transDecl $ f y
    where
        (com,rest) = span (== ',') xs
        f (HSE.TypeSig s [Ident _] ty) = HSE.TypeSig s [Ident $ '(':com++")"] ty

parseLine line x = Left $ show line ++ ":failed to parse: " ++ x

exts = map EnableExtension
    [EmptyDataDecls,TypeOperators,ExplicitForAll,GADTs,KindSignatures,MultiParamTypeClasses
    ,TypeFamilies,FlexibleContexts,FunctionalDependencies,ImplicitParams,MagicHash,UnboxedTuples]


---------------------------------------------------------------------
-- TRANSLATE THINGS

transDecl :: Decl -> Items
transDecl (GDataDecl s dat ctxt name bind _ [] _) = transDecl $ DataDecl s dat ctxt name bind [] []
transDecl (GDataDecl _ _ _ _ _ _ [GadtDecl s name _ ty] _) = transDecl $ HSE.TypeSig s [name] ty
transDecl x = IDecl x
