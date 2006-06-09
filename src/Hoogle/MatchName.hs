{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

module Hoogle.MatchName(
    NameTable,
    buildName, -- build a name table
    lookupName -- lookup a name
    ) where


import Hoogle.Result
import Hoogle.TypeSig

import Data.Char
import Data.List
import Data.Maybe


-- | The abstract data type
data NameTable = NameTable [(String, String, Result, Bool)]
                 deriving Show

-- | build a 'NameTable'
buildName :: [(ModuleName, Item)] -> NameTable
buildName xs = NameTable $ map f xs
    where
        lcase = map toLower
    
        f (_, Module x) = (lcase name, name,
            Result (Str $ showModuleName (init x)) (Str $ last x)
                   (Tag "u" $ Str "module") "module" [] 0 0,
            False)
            where name = last x
                   
        
        f (modu, x) = (lcase name, name,
            Result (Str $ showModuleName modu) (Str $ getName x)
                   (getType x) (getMode x) [] 0 (0 - length modu),
            head (asString x) == '(')
            where name = getName x


        getName x = noBracket $ asString x
        
        getType (Func _ x) = Str $ showConType x
        getType (Keyword x) = Tag "u" $ Str "keyword"
        getType (Class x) =
            Tags [Tag "u" $ Str "class", Str $ " " ++ showConType x]
        getType (TypeAlias name args _) =
            Tags [Tag "u" $ Str "type", Str $ concatMap (' ':) (name:args)]
        getType (Data b x) = 
            Tags [Tag "u" $ Str (if b then "newtype" else "data"), Str $ " " ++ showConType x]
        
        getMode (Func{}) = "func"
        getMode (Keyword{}) = "keyword"
        getMode (Class{}) = "class"
        getMode (TypeAlias{}) = "type"
        getMode (Data b x) = if b then "newtype" else "data"



noBracket ('(':xs) = init xs
noBracket x = x


-- | lookup an entry in a 'NameTable'
lookupName :: NameTable -> String -> [Result]
lookupName (NameTable xs) find = catMaybes $ map f xs
    where
        findc = noBracket find
        find2 = map toLower findc
    
        f (fnd, orig, res, b) =
            do (reason, pos) <- getMatch fnd orig
               return $ res{
                   resultName = brack b $ h pos (length find) (fromStr (resultName res)),
                   resultInfo = [ReasonText reason],
                   resultScore = score [ReasonText reason]
                 }
                            
        fromStr (Str x) = x

        getMatch :: String -> String -> Maybe (TextAmount, Int)
        getMatch fnd orig
            | findc == orig = Just (TextFullCase, 0)
            | find2 == fnd  = Just (TextFull, 0)
            | findc `isPrefixOf` orig = Just (TextPrefixCase, 0)
            | find2 `isPrefixOf` fnd  = Just (TextPrefix, 0)
            | find2 `isSuffixOf` fnd  = Just (TextSuffix, length fnd - length find2)
            | otherwise = g fnd 0
                     
              
        g [] n = Nothing
        g xs n | find2 `isPrefixOf` xs = Just (TextSome, n)
               | otherwise = g (tail xs) (n+1)
                   
                   
        h pos len xs = Tags $ [
                Str $ take pos2 xs,
                Tag "b" $ Str $ take len $ drop pos2 xs,
                Str $ drop (len+pos2) xs]
            where pos2 = pos + (if head xs == '(' then 1 else 0)


        brack True x = Tags [Str "(", x, Str ")"]
        brack _ x = x