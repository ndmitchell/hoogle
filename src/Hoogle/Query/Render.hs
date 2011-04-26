
module Hoogle.Query.Render(renderQuery) where

import General.Base
import Data.Generics.Uniplate
import Hoogle.Query.Type
import Hoogle.Type.All


-- | Render a query, in particular using 'TagColor' for any type signature argument positions.
renderQuery :: Query -> TagStr
renderQuery x = Tags $ namesig ++ [Str " " | namesig /= [] && scp /= []] ++ scp
    where
        namesig = case (null (names x), isNothing (typeSig x)) of
                      (True, True) -> []
                      (True, False) -> [Str ":: " | namelike] ++ showType
                      (False, True) -> showName 
                      _ -> showName ++ [Str " :: "] ++ showType
            where namelike = and [isAlpha y || isSpace y | Str xs <- universe $ Tags showType
                                                         , y:ys <- [dropWhile isSpace xs]]
        
        showName = intersperse (Str " ") $ map (TagBold . Str) (names x)
        showType = [renderTypeSig $ fromJust $ typeSig x]

        scp = [Str $ unwords $ map f $ scope x | scope x /= []]
        f (Scope b _ x) = (if b then "+" else "-") ++ x


renderTypeSig :: TypeSig -> TagStr
renderTypeSig (TypeSig con args) = Tags $
    Str (showConstraint con) :
    intersperse (Str " -> ")
       (zipWith TagColor [1..] (map (Str . showFun) finit) ++
        [TagColor 0 $ Str $ showFun flast])
    where
        (finit, flast) = (init funcs, last funcs)
        funcs = splitFun args
