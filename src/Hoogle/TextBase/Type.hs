
module Hoogle.TextBase.Type
    (TextBase, TextItem(..), Fact(..)
    ,itemPackage, itemModule, itemKeyword, itemClass, itemFunc, itemAlias, itemData, itemInstance
    ) where

import Hoogle.TypeSig.All
import General.Util
import Data.TagStr
import Data.Char
import Data.Generics.Uniplate


type TextBase = ([Fact], [TextItem])

-- FIXME: Should be moved inside Item.Type, probably
data TextItem = TextItem
    {itemLevel :: Int -- 0 = package, 1 = module, >2 = entry
    ,itemName :: [String] -- for modules is the module name, for all others is a singleton
    ,itemType :: Maybe TypeSig
    ,itemDisp :: TagStr -- TagColor 0 for result type, TagColor 1.. for arg types, TagBold for name
    ,itemURL :: URL
    ,itemDocs :: String
    }

data Fact
    = FactAlias TypeSig TypeSig
    | FactInstance TypeSig
    | FactDataKind String Int
    | FactClassKind String Int
    | FactCtorType String String -- Ctor, Data



---------------------------------------------------------------------
-- FIXME: Should be moved in to TextBase.Parser

textItem = TextItem 2 [] Nothing (Str "") "" ""

fact x y = (x,[y])

itemPackage x = fact [] $ textItem{itemLevel=0, itemName=[x],
    itemDisp=Tags [under "package",space,bold x]}

itemModule xs = fact [] $ textItem{itemLevel=1, itemName=xs,
    itemDisp=Tags [under "module",Str $ " " ++ concatMap (++".") (init xs),bold $ last xs]}

itemKeyword x = fact [] $ textItem{itemName=[x],
    itemDisp=Tags [under "keyword",space,bold x]}

itemClass x = fact (kinds True x) $ textItem{itemName=[a],
    itemDisp=Tags $ [under "class",space,b]}
    where (a,b) = typeHead x

itemFunc nam typ@(TypeSig _ ty) = fact (ctr++kinds False typ) $ textItem{itemName=[nam],itemType=Just typ,
    itemDisp=Tags[bold (operator nam), Str " :: ",renderTypeSig typ]}
    where operator xs@(x:_) | not $ isAlpha x || x `elem` "#_'" = "(" ++ xs ++ ")"
          operator xs = xs
          ctr = [FactCtorType nam y | isUpper $ head nam, TLit y <- [fst $ fromTApp $ last $ fromTFun ty]]

itemAlias from to = fact (FactAlias from to:kinds False from++kinds False to) $ textItem{itemName=[a],
    itemDisp=Tags[under "type",space,b]}
    where (a,b) = typeHead from

itemData d t = fact (kinds False t) $ textItem{itemName=[a],
    itemDisp=Tags[under (if d then "data" else "newtype"),space,b]}
    where (a,b) = typeHead t

itemInstance t = (FactInstance t:kinds True t, [])


under = TagUnderline . Str
bold = TagBold . Str
space = Str " "


typeHead :: TypeSig -> (String, TagStr)
typeHead (TypeSig con sig) = (a, Tags [Str $ showConstraint con, bold a, Str b])
    where (a,b) = break (== ' ') $ show sig


-- collect the kind facts, True for the outer fact is about a class
kinds :: Bool -> TypeSig -> [Fact]
kinds cls (TypeSig x y) = concatMap (f True) x ++ f cls y
    where
        f cls (TApp (TLit c) ys) = add cls c (length ys) ++
                                   if cls then [] else concatMap (f False) ys
        f cls (TLit c) = add cls c 0
        f cls x = if cls then [] else concatMap (f False) $ children x

        add cls c i = [(if cls then FactClassKind else FactDataKind) c i | not $ isTLitTuple c]
