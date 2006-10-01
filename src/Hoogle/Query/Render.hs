
module Hoogle.Query.Render(renderQuery) where

import Data.Maybe
import Data.List
import Data.Char

import General.All
import Hoogle.Query.Type
import Hoogle.TypeSig.All


renderQuery :: Query -> TagStr
renderQuery x = Tags $ namesig ++ scp ++ itms ++ flgs
    where
        namesig = case (null (names x), isNothing (typeSig x)) of
                      (True, True) -> []
                      (True, False) -> showType
                      (False, True) -> showName 
                      _ -> showName ++ [Str " :: "] ++ showType
        
        showName = intersperse (Str " ") $ map (TagBold . Str) (names x)
        showType = Str (showConstraint con) :
                   intersperse (Str " -> ") 
                       (zipWith TagColor [0..] (map (Str . show) finit) ++
                        [Str (show flast)])
            where
                Just (TypeSig con args) = typeSig x
                (finit, flast) = (init funcs, last funcs)
                funcs = case args of
                            TFun xs -> xs
                            xs -> [xs]
        
        scp = []
        itms = []
        
        
        flgs = concatMap (\x -> [Str " ", Str (f x)]) (flags x)
            where
                f (Count n) = "/count=" ++ show n
                f (Path x) = "/path=" ++ ['\"'|b] ++ x ++ ['\"'|b]
                    where b = any isSpace x
                f x = '/' : map toLower (show x)
