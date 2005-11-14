{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}


module Hoogle.MatchType(
    TypeTable,
    buildType,
    lookupType,
    compareTypes
    ) where


import Hoogle.TypeSig
import Hoogle.Result
import Hoogle.Parser
import Hoogle.Lexer
import Hoogle.MatchClass
import Hoogle.General

import Data.Maybe
import Data.List


data TypeTable = TypeTable [(ModuleName, Item, TypeCode)]
                 deriving Show


data TypeCode = TypeCode Int [Rule]
                deriving Show


data Rule = FreeSet [Var]
          | DataBind String Var
          | ClassBind String [Var]


data Var = Var Int [Int]


instance Show Var where
    show (Var n xs) = "@" ++ show n ++ concatMap (\x -> '.':show x) xs


instance Show Rule where
    show (FreeSet x) = "{" ++ (concat $ intersperse " " $ map show x) ++ "}"
    show (DataBind x y) = x ++ "=" ++ show y
    show (ClassBind x y) = x ++ "=>" ++ show y

           

buildType :: [(ModuleName, Item)] -> TypeTable
buildType x = TypeTable $ map f x
    where f (a, b) = (a, b, buildTypeCode $ typ b)


unpackSpine :: ConType -> (Constraint, [Type])
unpackSpine (c, TList (TLit "->":xs)) = (c, xs)
unpackSpine (c, x) = (c, [x])


buildTypeCode :: ConType -> TypeCode
buildTypeCode (con,typ) = TypeCode (length args) $ map asBound bound ++ concatMap asFree frees
    where
        frees = groupBy eqVar $ sortBy cmpVar free
        (free, bound) = partition (isTVar . snd) binds
        binds = concat $ zipWith f (map (\x -> Var x []) [0..]) (last args : init args)
        
        asBound (var, TLit x) = DataBind x var
        
        -- TODO: Multiparameter type classes
        asFree x = FreeSet items : [ClassBind c [i] | i <- items, c <- classes]
            where
                items = map fst x
                ((_,TVar var):_) = x
                classes = [r | TList [TLit r,TVar v] <- con, v == var]
        
        f :: Var -> Type -> [(Var, Type)]
        f var@(Var v vs) (TList (x:xs)) = (var, x) : (concat $ zipWith g [1..] xs)
            where g n x = f (Var v (vs ++ [n])) x
            
        f var x = [(var, x)]
    
        args = snd $ unpackSpine (con,typ)
        
        eqVar  (_, TVar a) (_, TVar b) = a == b
        cmpVar (_, TVar a) (_, TVar b) = compare a b


compareTypes :: ClassTable -> ConType -> ConType -> [[Reason]]
compareTypes classes left right = [a ++ b | a <- as, b <- bs]
    where
        as = map (map ReasonLeft  . thd3) $ checkType classes cleft right
        bs = map (map ReasonRight . thd3) $ checkType classes cright left
        thd3 (_,_,x) = x
    
        cleft = buildTypeCode left
        cright = buildTypeCode right


lookupType :: ClassTable -> TypeTable -> ConType -> [Result]
lookupType classes (TypeTable types) find = mapMaybe f types
    where
        findCode = buildTypeCode find
        
        f (modu, item, code) = do (order, a) <- checkTypeOne classes code find
                                  (_,     b) <- checkTypeOne classes findCode (typ item)
                                  let reasons = map ReasonLeft a ++ map ReasonRight b
                                  return $ Result
                                      (Str $ showModuleName modu)
                                      (Str $ name item)
                                      (showTypeTags (typ item) order)
                                      "func"
                                      reasons
                                      (score reasons)
                                      (0 - length modu)


checkTypeOne :: ClassTable -> TypeCode -> ConType -> Maybe ([Int], [MatchAmount])
checkTypeOne ct tc typ = if null res then Nothing else Just (b, c)
    where
        (a,b,c) = maximumBy f res
        res = checkType ct tc typ
        
        f (a,_,_) (b,_,_) = compare a b


checkType :: ClassTable -> TypeCode -> ConType -> [(Score, [Int], [MatchAmount])]
checkType classes code@(TypeCode n xs) typ = if abs posExtraArgs > 2 then [] else res
    where
        extraArgs = n - length types
        posExtraArgs = abs extraArgs
        badArgs = replicate extraArgs ArgExtra
    
        f (x:xs) = xs ++ [x]
        
        res = [(score reasons, f $ map fst arg, badArgs ++ reasons) | arg <- args,
               Just reasons <- [applyType classes code cons (map snd arg)]]
    
        (cons, types) = unpackSpine typ
        types2 = zip [1..] types
        lastType = last types2
        initType = init types2
        addTypes = initType ++ replicate extraArgs (0, TVar "_")
        
        args = map (lastType:) $ concatMap permute $ selection (n-1) addTypes

        

applyType :: ClassTable -> TypeCode -> Constraint -> [Type] -> Maybe [MatchAmount]
applyType classes (TypeCode n xs) cons types = if any isNothing res then Nothing
                                       else Just (concatMap fromJust res)
    where
        res = map f xs

        f :: Rule -> Maybe [MatchAmount]
        f (ClassBind x y) = do items <- mapMaybeAll getElement y
                               lookupClass classes cons x items
        
        f (DataBind x y) = case rootCtor y of
                               Nothing -> Nothing
                               Just "" -> Just [DataTooFree]
                               Just z -> if x == z then Just [] else Nothing
                               
        f (FreeSet x) = if Nothing `elem` roots || length roots > 2
                        then Nothing
                        else Just $ concat [tooSpecific, tooDifferent]
            where
                tooSpecific = if length roots == 2 then [DataTooSpecific] else []
                tooDifferent = replicate (length items - 1) FreeDifferent
            
                roots = nub $ Just "" : map rootCtor x
                items = nub $ mapMaybe f x
                
                f x = case fromJust (getElement x) of
                          TVar "_" -> Nothing
                          TVar x -> Just x
                          _ -> Nothing
        
        
        -- empty string means a free variable, i.e no explicit root
        -- Nothing means impossible
        rootCtor :: Var -> Maybe String
        rootCtor var = case getElement var of
                            Nothing -> Nothing
                            Just (TLit x) -> Just x
                            Just (TList (TLit x:xs)) -> Just x
                            x -> Just ""

        
        getElement :: Var -> Maybe Type
        getElement (Var n xs) = f xs (types !! n)
            where
                f [] x = Just x
                f (n:ns) (TList xs) | n < length xs = f ns (xs !! n)
                f (n:ns) (TVar x) = Just $ TVar "_"
                f _ _ = Nothing
        
        
