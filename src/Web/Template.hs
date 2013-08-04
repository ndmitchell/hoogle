{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Web.Template(
    main,
    escapeURL, escapeHTML,
    reload
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


getTemplate :: [Template] -> String -> Template
getTemplate ts x = case find ((==) x . templateName) ts of
    Nothing -> error $ "Could not find template " ++ x
    Just y -> y


---------------------------------------------------------------------
-- OUTPUT

-- Given a set of templates/args you need available, and a piece of sour
reload
    :: String -- ^ The source code
    -> [(String,[String])] -- ^ A set of templates/args you need avaialble
    -> [[String] -> String] -- ^ A list of functions which match the templates/args
reload src want = map f want
    where
        ts = resolve $ parse src

        f (name,args)
            | templateArgs t /= args = error $
                "Arguments for template " ++ name ++ " differ, expected " ++ show args ++ ", got " ++ show (templateArgs t)
            | otherwise = reloadTemplate t
            where t = getTemplate ts name


reloadTemplate :: Template -> ([String] -> String)
reloadTemplate t as = concatMap f $ templateContents t
    where
        atts = zip (templateArgs t) as
        f (Out x) = x
        f (Att e x) = escape e $ fromJust $ lookup x atts


---------------------------------------------------------------------
-- OUTPUT

generate :: String -> [Template] -> String
generate name xs = unlines $
    ["-- AUTO GENERATED - do not modify"
    ,"module " ++ name ++ "(Templates(..), defaultTemplates, loadTemplates) where"
    ,"import Web.Template"
    ,""
    ,"data Templates = Templates"] ++
    zipWith (++) ("  {":repeat "  ,")
         [templateName t ++ " :: " ++ intercalate " -> " (replicate (length (templateArgs t) + 1) "String") | t <- ts] ++
    ["  }"
    ,""
    ,"defaultTemplates :: Templates"
    ,"defaultTemplates = Templates" ++ concatMap ((++) " _" . templateName) ts
    ,""
    ,"loadTemplates :: String -> Templates"
    ,"loadTemplates x = Templates" ++ concatMap ((++) " _" . templateName) ts
    ,"    where"
    ,"        [" ++ intercalate "," (map ((++) "__" . templateName) ts) ++ "] = reload x $"] ++
    ["            " ++ show (templateName t, templateArgs t) ++ " :" | t <- ts] ++
    ["            []"] ++
    ["        _" ++ unwords (templateName t:templateArgs t) ++
        " = __" ++ templateName t ++ " [" ++ intercalate "," (templateArgs t) ++ "]" | t <- ts] ++
    concatMap generateTemplate ts
    where
        ts = nubBy ((==) `on` templateName) $ filter templateExport xs

generateTemplate :: Template -> [String]
generateTemplate Template{..} = "" :
        (unwords (('_':templateName) : templateArgs) ++ " = \"\"") :
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
        f (Call x) = concatMap f $ templateContents $ getTemplate args x
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
