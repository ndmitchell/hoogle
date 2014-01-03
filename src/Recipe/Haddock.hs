{-# LANGUAGE PatternGuards #-}

module Recipe.Haddock(
    haddockToHTML, haddockHacks
    ) where

import General.Base
import General.Web
import qualified Text.Read as R


data Chunk = Verb [String] | Blk [String] | Li [String] | Numb [String] | Defn [(String,String)] | Para String deriving (Ord,Eq)

haddockToHTML :: String -> [String]
haddockToHTML = intercalate [""] . map (concatMap linewrap . convert) . join . map classify . paragraphs . lines
    where
        empty = all isSpace
        para = unwords . map trim

        paragraphs = filter (not . all empty) . groupBy (\x y -> not (empty x) && not (empty y))
        
        classify xs = case trim (head xs) of
                        "@"    | trim (last xs) == "@", length xs > 1 -> Blk $ tail $ init xs
                            
                        '>':_  | all ((">" `isPrefixOf`) . ltrim) xs  -> Verb $ map (tail . ltrim) xs
                        
                        '[':ys | (cs, ']':zs) <- break (==']') ys     -> Defn [(trim cs, para $ zs : tail xs)]
                        
                        '*':ys                                        -> Li [para $ ys : tail xs]
                        '-':ys                                        -> Li [para $ ys : tail xs]
                        
                        '(':ys | (cs, ')':zs) <- break (==')') ys
                               , all isDigit cs                       -> Numb [para $ zs : tail xs]
                        c:ys | isDigit c
                             , '.':zs <- dropWhile isDigit ys         -> Numb [para $ zs : tail xs]
                        
                        _                                             -> Para $ para xs
                        
        join (Li xs   : Li ys   : zs) = join $ Li   (xs ++ ys) : zs
        join (Numb xs : Numb ys : zs) = join $ Numb (xs ++ ys) : zs
        join (Defn xs : Defn ys : zs) = join $ Defn (xs ++ ys) : zs
        join (x : ys)                 = x : join ys
        join []                       = []

        convert (Verb xs) = ["<pre>"] ++ map escapeHTML xs ++ ["</pre>"]
        convert (Blk  xs) = ["<pre>"] ++ map parseInline xs ++ ["</pre>"]
        convert (Li   xs) = ["<ul>"] ++ ["<li>" ++ x ++ "</li>" | x <- map parseInline xs] ++ ["</ul>"]
        convert (Numb xs) = convert $ Li xs
        convert (Defn xs) = intersperse "" [parseInline a ++ ": " ++ parseInline b | (a,b) <- xs]
        convert (Para s)  = [parseInline s]

        linewrap x | length x > 80 = (a ++ c) : linewrap (drop 1 d)
            where (a,b) = splitAt 60 x
                  (c,d) = break (== ' ') b
        linewrap x = [x | x /= ""]


parseInline :: String -> String
parseInline = concat . bits
    where
        tag x y = "<" ++ x ++ ">" ++ y ++ "</" ++ x ++ ">"

        table = [("@", "@",    Just . tag "tt" . parseInline)
                ,("/", "/",    Just . tag "i" . parseInline)
                ,("<", ">",    check (not . any isSpace) (tag "a"))
                ,("\"","\"",   check isModuleName (tag "a"))
                ,("\'","\'",   check isQName (tag "a"))]

        check f g s = if f s then Just (g s) else Nothing
        sel1 (a,_,_) = a

        bits :: String -> [String]
        bits xs | (st,end,mk):_ <- filter (flip isPrefixOf xs . sel1) table
                , xs <- drop (length st) xs
                , Just (now,next) <- close "" end xs
                , Just r <- mk (reverse now)
                = r : bits next
        bits ('\\':x:xs) = escapeHTML [x] : bits xs
        bits (x:xs) = escapeHTML [x] : bits xs
        bits [] = []

        close acc end xs | end `isPrefixOf` xs = Just (acc, drop (length end) xs)
        close acc end ('\\':x:xs) = close (x:'\\':acc) end xs
        close acc end (x:xs)      = close (x:acc) end xs
        close acc end ""          = Nothing


isModuleName :: String -> Bool
isModuleName = all ok . splitModuleString
    where
        ok s | [(R.Ident (y:ys), "")] <- R.readPrec_to_S R.lexP 0 s = isUpper y
        ok _ = False

splitModuleString :: String -> [String]
splitModuleString = wordsBy (== '.')

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f xs = case dropWhile f xs of
                   [] -> []
                   ys -> w : wordsBy f zs
                       where (w, zs) = break f ys

isQName :: String -> Bool
isQName xs = case R.readPrec_to_S R.lexP 0 xs of
                  [(R.Ident (y:ys), '.':zs)] | isUpper y -> isQName zs
                  [(R.Ident ys, "")]                     -> True
                  [(R.Symbol ys, "")]                    -> True
                  _                                      -> False


---------------------------------------------------------------------
-- HADDOCK HACKS

-- Eliminate @version
-- Change :*: to (:*:), Haddock bug
-- Change !!Int to !Int, Haddock bug
-- Change instance [overlap ok] to instance, Haddock bug
-- Change instance [incoherent] to instance, Haddock bug
-- Change instance [safe] to instance, Haddock bug
-- Change !Int to Int, HSE bug
-- Drop {-# UNPACK #-}, Haddock bug
-- Drop everything after where, Haddock bug

haddockHacks :: Maybe URL -> [String] -> [String]
haddockHacks loc src = maybe id haddockPackageUrl loc (translate src)
    where
        translate :: [String] -> [String]
        translate = map (unwords . g . map f . words) . filter (not . isPrefixOf "@version ")

        f "::" = "::"
        f (':':xs) = "(:" ++ xs ++ ")"
        f ('!':'!':x:xs) | isAlpha x = xs
        f ('!':x:xs) | isAlpha x || x `elem` "[(" = x:xs
        f x | x `elem` ["[overlap","ok]","[incoherent]","[safe]"] = ""
        f x | x `elem` ["{-#","UNPACK","#-}"] = ""
        f x = x

        g ("where":xs) = []
        g (x:xs) = x : g xs
        g [] = []

haddockPackageUrl :: URL -> [String] -> [String]
haddockPackageUrl x = concatMap f
    where f y | "@package " `isPrefixOf` y = ["@url " ++ x, y]
              | otherwise = [y]
