
module Data.Binary.Defer.Derive where

import Data.Generics
import Data.List


derive :: (Typeable a, Show a, Data a) => a -> String
derive x = "instance " ++
           ['(' | nTypeChildren>1] ++ context ++ [')' | nTypeChildren>1] ++
           (if nTypeChildren > 0 then " => " else "") ++
           tyConString typeName ++ concatMap (' ':) typeLetters ++ " where\n" ++
           "    bothDefer = defer\n" ++
           "        [" ++ concat (intersperse "\n        ," alts) ++ "\n" ++
           "        ]\n"
    where
        context = concat $ intersperse ", " $ map ("BinaryDefer "++) typeLetters
        typeLetters = map (:[]) $ take nTypeChildren ['a'..]
        nTypeChildren = length typeChildren
        (typeName,typeChildren) = splitTyConApp (typeOf x)
        
        (alts, types) = unzip $ map genAlt $ dataTypeConstrs $ dataTypeOf x
        
        genAlt :: Constr -> (String, [String])
        genAlt con = (str, [])
            where
                str = "\\ ~(" ++ name ++ concatMap (' ':) lets ++ ") -> " ++
                      "unit " ++ name ++ concatMap (" << "++) lets
                
                name = showConstr con
                lets = map (:[]) $ take n ['a'..]
                n = length $ gmapQ undefined $ fromConstr con `asTypeOf` x
