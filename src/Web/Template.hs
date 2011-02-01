{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Web.Template(
    main,
    escapeURL, escapeHTML
    ) where

import General.Base
import General.System
import General.Web


main :: IO ()
main = do
    [from,to,modname] <- getArgs
    src <- readFile from
    writeFileBinary to $ generate modname $ resolve $ parse src


---------------------------------------------------------------------
-- TYPE

data Template = Template
    {templateName :: String
    ,templateArgs :: [String]
    ,templateExport :: Bool
    ,templateContents :: [Fragment]
    }

data Fragment
    = Out String -- ^ Output some text
    | Att Esc String -- ^ Output an attribute (and how to escape it)
    | Set String String -- ^ Set an attribute to a value
    | Call String -- ^ Call another template

data Esc = EscNone | EscHtml | EscUrl deriving Eq

escapeStr e = case e of EscHtml -> "escapeHTML "; EscUrl -> "escapeURL "; _ -> ""
escape e = case e of EscHtml -> escapeHTML; EscUrl -> escapeURL; _ -> id


joinOut (Out x:Out y:zs) = joinOut $ Out (x++y) : zs
joinOut (x:xs) = x : joinOut xs
joinOut [] = []

---------------------------------------------------------------------
-- OUTPUT

generate :: String -> [Template] -> String
generate name xs = unlines $
    ("module " ++ name ++ " where") :
    "import Web.Template" :
    concatMap generateTemplate (filter templateExport xs)

generateTemplate :: Template -> [String]
generateTemplate Template{..} = "" :
        (templateName ++ " :: " ++ concat (replicate (length templateArgs) "String -> ") ++ "IO String") :
        (unwords (templateName : templateArgs) ++ " = return $ \"\"") :
        map ((++) "  " . f) templateContents
    where
        f (Out x) = "++ " ++ show x
        f (Att e x) = "++ " ++ escapeStr e ++ x


---------------------------------------------------------------------
-- RESOLVE

-- | Eliminate Set and Call, fill in the template arguments
resolve :: [Template] -> [Template]
resolve xs = map (resolveFree . resolveSet . resolveCall xs) xs

resolveFree t = t{templateArgs=args}
    where seen = nub [x | Att _ x <- templateContents t]
          args = nub $ filter (`elem` seen) (templateArgs t) ++ seen

resolveSet t = t{templateContents = joinOut $ f [] $ templateContents t}
    where
        f seen (Set x y:xs) = f ((x,y):seen) xs
        f seen (Att e y:xs) | Just v <- lookup y seen = Out (escape e v) : f seen xs
        f seen (x:xs) = x : f seen xs
        f seen [] = []

resolveCall args t = t{templateContents = concatMap f $ templateContents t}
    where
        f (Call x) | Just t <- find ((==) x . templateName) args = concatMap f $ templateContents t
        f x = [x]


---------------------------------------------------------------------
-- PARSING

parse :: String -> [Template]
parse = f . dropWhile (not . isPrefixOf "#") . filter (not . all isSpace) . lines
    where
        f (x:xs) = Template name args exp (parseTemplate $ unlines a) : f b
            where (a,b) = break ("#" `isPrefixOf`) xs
                  ys = words $ dropWhile (== '#') x
                  exp = ["export"] `isPrefixOf` ys
                  name:args = if exp then tail ys else ys
        f [] = []


parseTemplate :: String -> [Fragment]
parseTemplate = f 
    where
        f [] = []
        f ('$':xs) = g a : f (drop 1 b)
            where (a,b) = break (== '$') xs
        f xs = Out a : f b
            where (a,b) = break (== '$') xs

        g ('!':xs) = Att EscNone xs
        g ('&':xs) = Att EscHtml xs
        g ('%':xs) = Att EscUrl xs
        g ('#':xs) = Call xs
        g xs | (a,'=':b) <- break (== '=') xs = Set a b
        g x = error $ "Templating error, perhaps you forgot the escape format? $" ++ x ++ "$"
