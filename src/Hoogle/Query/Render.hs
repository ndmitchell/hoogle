
module Hoogle.Query.Render(renderQuery) where

import General.Code
import Data.TagStr
import Data.Generics.Uniplate
import Hoogle.Query.Type
import Hoogle.Type.All


renderQuery :: Query -> TagStr
renderQuery x = Tags $ namesig ++ scp ++ itms
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

        scp = []
        itms = []
