{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module Output.Types(writeTypes, searchTypes, searchFingerprintsDebug) where

{-
Approach:
Each signature is stored, along with a fingerprint
A quick search finds the most promising 100 fingerprints
A slow search ranks the 100 items, excluding some
-}

import           Control.Applicative
import           Control.Monad.Extra
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Binary                      hiding (get, put)
import qualified Data.ByteString.Char8            as BS
import           Data.Data
import           Data.Generics.Uniplate.Data
import           Data.List.Extra
import qualified Data.Map.Strict                  as Map
import           Data.Maybe
import qualified Data.Set                         as Set
import           Data.STRef
import           Data.Tuple.Extra
import qualified Data.Vector.Storable             as V
import qualified Data.Vector.Storable.Mutable     as VM
import           Foreign.Storable
import           Numeric.Extra
import           Prelude
import           System.FilePath
import           System.IO.Extra

import           General.IString
import           General.Store
import           General.Str
import           General.Util
import           Input.Item


writeTypes :: StoreWrite -> Maybe FilePath -> [(Maybe TargetId, Item)] -> IO ()
writeTypes store debug xs = do
    let debugger ext body = whenJust debug $ \file -> writeFileUTF8 (file <.> ext) body
    inst <- return $ Map.fromListWith (+) [(fromIString x,1) | (_, IInstance (Sig _ [TCon x _])) <- xs]
    xs <- writeDuplicates store [(i, fromIString <$> t) | (Just i, ISignature t) <- xs]
    names <- writeNames store debugger inst xs
    xs <- return $ map (lookupNames names (error "Unknown name in writeTypes")) xs
    writeFingerprints store xs
    writeSignatures store xs

searchTypes :: StoreRead -> Sig String -> [TargetId]
searchTypes store q = take nMatches (concat [ search qry' | variant <- variants, qry' <- variant qry ])
    where
        nMatches = 100
        qry = lookupNames names name0 (strPack <$> q) -- map unknown fields to name0, i.e. _
        names = readNames store
        search = concatMap (expandDuplicates $ readDuplicates store) . searchTypeMatch db getSig arrow nMatches
        db  = zip (readSignatureIndex store)
                  (V.toList $ storeRead store TypesFingerprints :: [Fingerprint])
        getSig = readSignatureAt store
        arrow = lookupCtor store names "->"

        -- Different variations on the search query. Each variation is run in turn until we've gathered
        -- 100 hits or run out of variations to try.
        variants = [ pure, permuted, partial, partial >=> permuted ]

        -- Permute the arguments of a two-argument query.
        permuted qq = case sigTy qq of
            [a1, a2, r] -> [ qq { sigTy = [a2, a1, r] } ]
            _           -> []

        -- Add a `Maybe` to the query's result type.
        partial  qq = case sigTy qq of
            []  -> []
            tys -> [ qq { sigTy = init tys ++ [TCon maybeCtor [last tys]] } ]

        maybeCtor = lookupCtor store names "Maybe"

lookupCtor :: StoreRead -> Names -> String -> Name
lookupCtor store names c =
    case sigTy (lookupNames names name0 s) of
        [TCon n _] -> n
        _          -> name0
    where
      s = strPack <$> Sig { sigCtx = [], sigTy = [TCon c []] }

searchFingerprintsDebug :: StoreRead -> (String, Sig String) -> [(String, Sig String)] -> [String]
searchFingerprintsDebug store query answers = intercalate [""] $
    f False "Query" query : zipWith (\i -> f True ("Answer " ++ show i)) [1..] answers
    where
        qsig = lookupNames names name0 $ strPack <$> snd query
        names = readNames store

        f match name (raw, sig) =
            [name ++ ": " ++ raw
            ,"Sig String: " ++ prettySig sig
            ,"Sig Name: " ++ prettySig (fmap prettyName sn)
            ,"Fingerprint: " ++ prettyFingerprint fp] ++
            if not match then [] else
            ["Cost: " ++ maybe "X, no match" show (matchFingerprint qsig fp)
            ,"Explain: " ++ showExplain (matchFingerprintDebug qsig fp)]
            where
                sn = lookupNames names name0 $ strPack <$> sig
                fp = toFingerprint sn

                showExplain = intercalate ", " . map g . sortOn (either (const minBound) (negate . snd))
                g (Left s)       = "X " ++ s
                g (Right (s, x)) = show x ++ " " ++ s


---------------------------------------------------------------------
-- NAME/CTOR INFORMATION

data TypesNames a where TypesNames :: TypesNames (BStr0, V.Vector Name) deriving Typeable

-- Must be a unique Name per String.
-- First 0-99 are variables, rest are constructors.
-- More popular type constructors have higher numbers.
-- There are currently about 14K names, so about 25% of the bit patterns are taken
newtype Name = Name Word16 deriving (Eq,Ord,Show,Data,Typeable,Storable,Binary)

name0 = Name 0 -- use to represent _

isCon, isVar :: Name -> Bool
isVar (Name x) = x < 100
isCon = not . isVar

prettyName :: Name -> String
prettyName x@(Name i)
    | x == name0 = "_"
    | isVar x = "v" ++ show i
    | otherwise = "C" ++ show i


-- | Give a name a popularity, where 0 is least popular, 1 is most popular
popularityName :: Name -> Double
popularityName (Name n) | isVar $ Name n = error "Can't call popularityName on a Var"
                        | otherwise = fromIntegral (n - 100) / fromIntegral (maxBound - 100 :: Word16)

newtype Names = Names {lookupName :: Str -> Maybe Name}

lookupNames :: Names -> Name -> Sig Str -> Sig Name
lookupNames Names{..} def (Sig ctx typ) = Sig (map f ctx) (map g typ)
    where
        vars = nubOrd $ strPack "_" : [x | Ctx _ x <- ctx] ++ [x | TVar x _ <- universeBi typ]
        var x = Name $ min 99 $ fromIntegral $ fromMaybe (error "lookupNames") $ elemIndex x vars
        con = fromMaybe def . lookupName

        f (Ctx a b) = Ctx (con $ strCons '~' a) (var b)
        g (TCon x xs) = TCon (con x) $ map g xs
        g (TVar x xs) = TVar (var x) $ map g xs


writeNames :: StoreWrite -> (String -> String -> IO ()) -> Map.Map Str Int -> [Sig Str] -> IO Names
writeNames store debug inst xs = do
    let sigNames (Sig ctx typ) = nubOrd [strCons '~' x | Ctx x _ <- ctx] ++ nubOrd [x | TCon x _ <- universeBi typ]

    -- want to rank highly instances that have a lot of types, and a lot of definitions
    -- eg Eq is used and defined a lot. Constructor is used in 3 places but defined a lot.
    let freq :: Map.Map Str Int = -- how many times each identifier occurs
            Map.unionWith (\typ sig -> sig + min sig typ) (Map.mapKeysMonotonic (strCons '~') inst) $
            Map.fromListWith (+) $ map (,1::Int) $ concatMap sigNames xs
    let names = spreadNames $ Map.toList freq
    debug "names" $ unlines [strUnpack s ++ " = " ++ show n ++ " (" ++ show (freq Map.! s) ++ " uses)" | (s,n) <- names]
    names <- return $ sortOn fst names
    storeWrite store TypesNames (bstr0Join $ map (strUnpack . fst) names, V.fromList $ map snd names)
    let mp2 = Map.fromAscList names
    return $ Names $ \x -> Map.lookup x mp2


-- | Given a list of names, spread them out uniquely over the range [Name 100 .. Name maxBound]
--   Aim for something with a count of p to be at position (p / pmax) linear interp over the range
spreadNames :: [(a, Int)] -> [(a, Name)]
spreadNames [] = []
spreadNames (sortOn (negate . snd) -> xs@((_,limit):_)) = check $ f (99 + fromIntegral (length xs)) maxBound xs
    where
        check xs | all (isCon . snd) xs && length (nubOrd $ map snd xs) == length xs = xs
                 | otherwise = error "Invalid spreadNames"

        -- I can only assign values between mn and mx inclusive
        f :: Word16 -> Word16 -> [(a, Int)] -> [(a, Name)]
        f !mn !mx [] = []
        f mn mx ((a,i):xs) = (a, Name real) : f (mn-1) (real-1) xs
            where real = fromIntegral $ max mn $ min mx ideal
                  ideal = mn + floor (fromIntegral (min commonNameThreshold i) * fromIntegral (mx - mn) / fromIntegral (min commonNameThreshold limit))

-- WARNING: Magic constant.
-- Beyond this count names don't accumulate extra points for being common.
-- Ensures that things like Bool (4523 uses) ranks much higher than ShakeOptions (24 uses) by not having
-- [] (10237 uses) skew the curve too much and use up all the available bits of discrimination.
commonNameThreshold = 1024

readNames :: StoreRead -> Names
readNames store = Names $ \x -> Map.lookup (bstrPack $ strUnpack x) mp
    where mp = Map.fromAscList $ zip (bstr0Split s) $ V.toList n
          (s, n) = storeRead store TypesNames


---------------------------------------------------------------------
-- DUPLICATION INFORMATION

data TypesDuplicates a where TypesDuplicates :: TypesDuplicates (Jagged TargetId) deriving Typeable

newtype Duplicates = Duplicates {expandDuplicates :: Int -> [TargetId]}

-- writeDuplicates xs == nub (map snd xs)
    -- all duplicates are removed, order of first element is preserved
-- (i,x) <- zip [0..] (writeDuplicates xs); expandDuplicates i == map fst (filter ((==) x . snd) xs)
    -- given the result at position i, expandDuplicates gives the TargetId's related to it

writeDuplicates :: Ord a => StoreWrite -> [(TargetId, Sig a)] -> IO [Sig a]
writeDuplicates store xs = do
    -- s=signature, t=targetid, p=popularity (incoing index), i=index (outgoing index)
    xs <- return $ map (second snd) $ sortOn (fst . snd) $ Map.toList $
        Map.fromListWith (\(x1,x2) (y1,y2) -> (, x2 ++ y2) $! min x1 y1)
                         [(s,(p,[t])) | (p,(t,s)) <- zip [0::Int ..] xs]
    -- give a list of TargetId's at each index
    storeWrite store TypesDuplicates $ jaggedFromList $ map (reverse . snd) xs
    return $ map fst xs

readDuplicates :: StoreRead -> Duplicates
readDuplicates store = Duplicates $ V.toList . ask
    where ask = jaggedAsk $ storeRead store TypesDuplicates


---------------------------------------------------------------------
-- FINGERPRINT INFORMATION

data TypesFingerprints a where TypesFingerprints :: TypesFingerprints (V.Vector Fingerprint) deriving Typeable

data Fingerprint = Fingerprint
    {fpRare1 :: {-# UNPACK #-} !Name -- Most rare ctor, or 0 if no rare stuff
    ,fpRare2 :: {-# UNPACK #-} !Name -- 2nd rare ctor
    ,fpRare3 :: {-# UNPACK #-} !Name -- 3rd rare ctor
    ,fpArity :: {-# UNPACK #-} !Word8 -- Artiy, where 0 = CAF
    ,fpTerms :: {-# UNPACK #-} !Word8 -- Number of terms (where 255 = 255 and above)
    } deriving (Eq,Show,Typeable)

prettyFingerprint :: Fingerprint -> String
prettyFingerprint Fingerprint{..} =
    "arity=" ++ show fpArity ++ ", terms=" ++ show fpTerms ++
    ", rarity=" ++ unwords (map prettyName [fpRare1, fpRare2, fpRare3])


{-# INLINE fpRaresFold #-}
fpRaresFold :: (b -> b -> b) -> (Name -> b) -> Fingerprint -> b
fpRaresFold g f Fingerprint{..} = f fpRare1 `g` f fpRare2 `g` f fpRare3

instance Storable Fingerprint where
    sizeOf _ = 64
    alignment _ = 4
    peekByteOff ptr i = Fingerprint
        <$> peekByteOff ptr (i+0) <*> peekByteOff ptr (i+2) <*> peekByteOff ptr (i+4)
        <*> peekByteOff ptr (i+6) <*> peekByteOff ptr (i+7)
    pokeByteOff ptr i Fingerprint{..} = do
        pokeByteOff ptr (i+0) fpRare1 >> pokeByteOff ptr (i+2) fpRare2 >> pokeByteOff ptr (i+4) fpRare3
        pokeByteOff ptr (i+6) fpArity >> pokeByteOff ptr (i+7) fpTerms

toFingerprint :: Sig Name -> Fingerprint
toFingerprint sig = Fingerprint{..}
    where fpRare1:fpRare2:fpRare3:_ = sort (nubOrd $ filter isCon $ universeBi sig) ++ [name0,name0,name0]
          fpArity = fromIntegral $ min 255 $ max 0 $ pred $ length $ sigTy sig
          fpTerms = fromIntegral $ min 255 $ length (universeBi sig :: [Name])

writeFingerprints :: StoreWrite -> [Sig Name] -> IO ()
writeFingerprints store xs = storeWrite store TypesFingerprints $ V.fromList $ map toFingerprint xs

data MatchFingerprint a ma = MatchFingerprint
    {mfpAdd  :: a -> a -> a
    ,mfpAddM :: ma -> ma -> ma
    ,mfpJust :: a -> ma
    ,mfpCost :: String -> Int -> a
    ,mfpMiss :: String -> ma
    }

matchFingerprint :: Sig Name -> Fingerprint -> Maybe Int
matchFingerprint = matchFingerprintEx MatchFingerprint{..}
    where
        mfpAdd = (+)
        mfpAddM = liftM2 (+)
        mfpJust = Just
        mfpCost _ x = x
        mfpMiss _ = Nothing

matchFingerprintDebug :: Sig Name -> Fingerprint -> [Either String (String, Int)]
matchFingerprintDebug = matchFingerprintEx MatchFingerprint{..}
    where
        mfpAdd = (++)
        mfpAddM = (++)
        mfpJust = id
        mfpCost s x = [Right (s,x)]
        mfpMiss s = [Left s]


{-# INLINE matchFingerprintEx #-}
matchFingerprintEx :: forall a ma . MatchFingerprint a ma -> Sig Name -> Fingerprint -> ma -- lower is better
matchFingerprintEx MatchFingerprint{..} sig@(toFingerprint -> target) =
    \candidate -> arity (fpArity candidate) `mfpAddM` terms (fpTerms candidate) `mfpAddM` rarity candidate
    where
        -- CAFs must match perfectly, otherwise too many is better than too few
        arity | ta == 0 = \ca -> if ca == 0 then mfpJust $ mfpCost "arity equal" 0 else mfpMiss "arity different and query a CAF" -- searching for a CAF
              | otherwise = \ca -> case fromIntegral $ ca - ta of
                    _ | ca == 0 -> mfpMiss "arity different and answer a CAF" -- searching for a CAF
                    0  -> mfpJust $ mfpCost "arity equal" 0 -- perfect match
                    -1 -> mfpJust $ mfpCost "arity 1 to remove" 1000 -- not using something the user carefully wrote
                    n | n > 0 && allowMore -> mfpJust $ mfpCost ("arity " ++ show n ++ " to add with wildcard") $ 300 * n -- user will have to make up a lot, but they said _ in their search
                    1  -> mfpJust $ mfpCost "arity 1 to add" 300  -- user will have to make up an extra param
                    2  -> mfpJust $ mfpCost "arity 2 to add"  900  -- user will have to make up two params
                    _ -> mfpMiss ""
            where
                ta = fpArity target
                allowMore = TVar name0 [] `elem` sigTy sig

        -- missing terms are a bit worse than invented terms, but it's fairly balanced, clip at large numbers
        terms = \ct -> case fromIntegral $ ct - tt of
                n | abs n > 20 -> mfpMiss $ "terms " ++ show n ++ " different" -- too different
                  | n == 0 -> mfpJust $ mfpCost "terms equal" 0
                  | n > 0 -> mfpJust $ mfpCost ("terms " ++ show n ++ " to add") $ n * 10 -- candidate has more terms
                  | otherwise -> mfpJust $ mfpCost ("terms " ++ show (-n) ++ " to remove") $ abs n * 12 -- candidate has less terms
            where
                tt = fpTerms target

        -- given two fingerprints, you have three sets:
        -- Those in common; those in one but not two; those in two but not one
        -- those that are different
        rarity = \cr -> let tr = target in mfpJust $
                differences 5000 400 tr cr `mfpAdd` -- searched for T but its not in the candidate, bad if rare, not great if common
                differences 1000  50 cr tr          -- T is in the candidate but I didn't search for it, bad if rare, OK if common
            where
                fpRaresElem :: Name -> Fingerprint -> Bool
                fpRaresElem !x = fpRaresFold (||) (== x)

                differences :: Double -> Double -> Fingerprint -> Fingerprint -> a
                differences !rare !common !want !have = fpRaresFold mfpAdd f want
                    where f n | fpRaresElem n have = mfpCost ("term in common " ++ prettyName n) 0
                              | n == name0 = mfpCost ("term _ missing") $ floor rare -- should this be common?
                              | otherwise = let p = popularityName n in mfpCost ("term " ++ prettyName n ++ " (" ++ showDP 2 p ++ ") missing") $
                                            floor $ (p*common) + ((1-p)*rare)


---------------------------------------------------------------------
-- SIGNATURES

data TypesSigPositions a where TypesSigPositions :: TypesSigPositions (V.Vector Word32) deriving Typeable
data TypesSigData a where TypesSigData :: TypesSigData BS.ByteString deriving Typeable

writeSignatures :: StoreWrite -> [Sig Name] -> IO ()
writeSignatures store xs = do
    v <- VM.new $ length xs
    forM_ (zip [0..] xs) $ \(i,x) -> do
        let b = encodeBS x
        storeWritePart store TypesSigData b
        VM.write v i $ fromIntegral $ BS.length b
    v <- V.freeze v
    storeWrite store TypesSigPositions v

type SigLoc = (Word32, Word32)

readSignatureIndex :: StoreRead -> [SigLoc] -- (offset,size) pairs for each field
readSignatureIndex store = zip offsets (V.toList sizes)
  where sizes   = storeRead store TypesSigPositions
        offsets = V.toList $ V.prescanl' (+) 0 sizes

readSignatureAt :: StoreRead -> SigLoc -> Sig Name
readSignatureAt store (offset, size) = decodeBS (BS.take (fromIntegral size)
                                                 $ snd
                                                 $ BS.splitAt (fromIntegral offset) bs)
  where
    bs = storeRead store TypesSigData

---------------------------------------------------------------------
-- TYPE SEARCH

searchTypeMatch :: [(SigLoc, Fingerprint)]
                -> (SigLoc -> Sig Name)
                -> Name
                -> Int
                -> Sig Name
                -> [Int]
searchTypeMatch db getSig arrow n sig =
    map snd $ takeSortOn fst n
      [ (500 * v + fv, i) | (fv, (i, sigIdx, f)) <- bestByFingerprint
                          , v  <- maybeToList (test $ getSig sigIdx)]
    where bestByFingerprint = takeSortOn fst (max 5000 n)
            [ (fv, (i, sigIdx, f)) | (i, (sigIdx, f)) <- zip [0..] db
                                   , fv <- maybeToList (matchFp f) ]
          matchFp = matchFingerprint sig
          test = matchType arrow sig

matchType :: Name -> Sig Name -> Sig Name -> Maybe Int
matchType arr qry ans = unWork <$> lhs `matches` rhs
    where
      lhs = (toTyp arr qry, sigCtx qry)
      rhs = (toTyp arr ans, sigCtx ans)

-- Check if two types-with-context match, returning the amount of work
-- needed to create the match.
matches :: (Typ Name, [Ctx Name]) -> (Typ Name, [Ctx Name]) -> Maybe Work
matches (lhs, lctx) (rhs, rctx) = runST $ evalStateT (getWork go) (Work 0)
  where
    go :: forall s. StateT Work (ST s) Bool
    go = do
        -- Try to unify the answer type with the query type.
        (qry, qryC) <- lift (refTyp True  lhs lctx)
        (ans, ansC) <- lift (refTyp False rhs rctx)
        unifyTyp qry ans >>= \case
            False -> return False
            True  -> do
                -- Normalize constraints
                let normalize (Ctx c a) = lift (Ctx <$> getName c <*> getName a)
                qryNCs <- Set.fromList <$> (mapM normalize qryC)
                ansNCs <- Set.fromList <$> (mapM normalize ansC)

                nqry <- lift $ normalizeTy qry
                nans <- lift $ normalizeTy ans

                -- Discharge constraints; remove any answer-constraint that is also a query-constraint,
                -- and then remove any remaining answer-constraint that is constraining a concrete type.
                -- TODO: keep constrained concrete types but weight them differently if they correspond
                --       to a known instance (e.g. free if we know the instance, rather expensive otherwise).
                let addl = filter isAbstract (Set.toList $ ansNCs `Set.difference` qryNCs)
                    isAbstract (Ctx c a) = isVar a

                workDelta (Work (3 * length addl))

                return True

    getWork action = action >>= \case
        True  -> Just <$> get
        False -> return Nothing

    normalizeTy = \case
        TyVar n tys -> TyVar <$> getName n <*> mapM normalizeTy tys
        TyCon n tys -> TyCon <$> getName n <*> mapM normalizeTy tys
        TyFun args retn -> TyFun <$> mapM normalizeTy args <*> normalizeTy retn


-- A slight variation on 'Ty', with a special term for functions.
data Typ n
    = TyFun [Typ n] (Typ n)
    | TyCon n [Typ n]
    | TyVar n [Typ n]
  deriving (Eq, Ord, Functor)

-- Rebuild a little bit of recursion-schemes machinery for Typ.
data TypF n t
    = TyFunF [t] t
    | TyConF n [t]
    | TyVarF n [t]
  deriving (Eq, Ord, Functor)

unroll :: Typ n -> TypF n (Typ n)
unroll = \case
    TyFun args retn -> TyFunF args retn
    TyCon n tys     -> TyConF n tys
    TyVar n tys     -> TyVarF n tys

foldTy :: (TypF n a -> a) -> Typ n -> a
foldTy phi = phi . fmap (foldTy phi) . unroll

instance Show n => Show (Typ n) where
    show = foldTy $ \case
        TyFunF typs res -> "<" ++ intercalate ", " typs ++ "; " ++ res ++ ">"
        TyConF n args -> unwords (show n : args)
        TyVarF n args -> unwords (show n : args)

-- Convert a Sig to a Typ.
toTyp :: Name -> Sig Name -> Typ Name
toTyp arrow Sig{..} = case sigTy of
    [] -> error "no types?"
    tys -> let args = init tys
               retn = last tys
           in TyFun (map toTy args) (toTy retn)
  where
    toTy = \case
      TCon n []   | n == arrow -> TyCon n [] -- empty function type?!
      TCon n tys | n == arrow -> TyFun (map toTy (init tys)) (toTy $ last tys)
      TCon n tys -> TyCon n (map toTy tys)
      TVar n tys -> TyVar n (map toTy tys)


---------------------------------------------------------------------
-- UNIFICATION

-- A union-find data structure for names

type NameRef s = STRef s (NameInfo s)

data NameInfo s =
    NameInfo { niParent :: !(Maybe (NameRef s))
             , niRank   :: !Int
             , niName   :: !Name
             , niFree   :: !Bool
             }
  deriving Eq

-- Find the name of the equivalence class's (current) representative.
getName :: NameRef s -> ST s Name
getName ref = do
    rep <- findRep ref
    niName <$> readSTRef rep

-- Create a new name reference from a name. @fixed == True@ means
-- that the reference cannot be unified with any other fixed refs.
newNameInfo :: Bool -> Name -> ST s (STRef s (NameInfo s))
newNameInfo fixed n = newSTRef $
  NameInfo { niParent = Nothing
           , niRank   = 0
           , niName   = n
           , niFree   = not fixed && isVar n
           }

-- The "find" part of union-find, with path compression.
findRep :: NameRef s -> ST s (NameRef s)
findRep ref = do
    ni <- readSTRef ref
    case niParent ni of
        Nothing -> return ref
        Just p  -> do
            root <- findRep p
            writeSTRef ref (ni { niParent = Just root })
            return root

-- The "union" part of union-find, with union-by-rank.
-- Each unification is given a cost of 1 work unit.
unifyName :: NameRef s -> NameRef s -> StateT Work (ST s) Bool
unifyName lhs rhs = do
    lhs' <- lift $ findRep lhs
    rhs' <- lift $ findRep rhs
    lInfo <- lift $ readSTRef lhs'
    rInfo <- lift $ readSTRef rhs'
    let lFree = niFree lInfo
        rFree = niFree rInfo
        lName = niName lInfo
        rName = niName rInfo
    let ok = lFree || rFree || lName == rName
    when (ok && lInfo /= rInfo) $ do
        -- Union by rank, except prefer concrete names over type variables.
        workDelta (Work 1)
        let lRank = niRank lInfo
            rRank = niRank rInfo
        let (root, child) = if not lFree || lRank <= rRank
                            then (lhs', rhs')
                            else (rhs', lhs')
        lift $ modifySTRef' child (\n -> n { niParent = Just root })
        when (lRank == rRank) $ lift $ modifySTRef' root (\n -> n { niRank = lRank + 1 })

    return ok

-- Allocate new references for each name that appears in the type and context.
refTyp :: Bool -> Typ Name -> [Ctx Name] -> ST s (Typ (NameRef s), [Ctx (NameRef s)])
refTyp fixed t cs =
    evalStateT go (Map.fromList [])
  where
    go = do
        ty  <- mkRefs t
        ctx <- forM cs $ \(Ctx c a) -> Ctx <$> getRef c <*> getRef a
        return (ty, ctx)

    mkRefs = foldTy $ \case
        TyVarF n args    -> TyVar <$> getRef n <*> sequence args
        TyConF n args    -> TyCon <$> getRef n <*> sequence args
        TyFunF args retn -> TyFun <$> sequence args <*> retn

    getRef n = do
        known <- get
        case Map.lookup n known of
            Just ref -> return ref
            Nothing  -> do
                ref <- lift (newNameInfo fixed n)
                put (Map.insert n ref known)
                return ref

-- Unify two types.
unifyTyp :: Typ (NameRef s) -> Typ (NameRef s) -> StateT Work (ST s) Bool
unifyTyp lhs rhs = case (lhs, rhs) of
    (TyCon n tys, TyVar n' tys') | length tys == length tys' -> do
            ok <- unifyName n n'
            if not ok
              then return False
              else and <$> zipWithM unifyTyp tys tys'

    (TyCon n tys, TyCon n' tys') | length tys == length tys' -> do
            ok <- unifyName n n'
            if not ok
              then return False
              else and <$> zipWithM unifyTyp tys tys'

    (TyVar n tys, TyVar n' tys') | length tys == length tys' -> do
            ok <- unifyName n n'
            if not ok
              then return False
              else and <$> zipWithM unifyTyp tys tys'

    (TyFun args ret, TyFun args' ret') | length args == length args' -> do
            ok <- unifyTyp ret ret'
            if not ok
              then return False
              else and <$> zipWithM unifyTyp args args'

    _ -> return False

-- The total cost of a unification operation.
newtype Work = Work Int

unWork :: Work -> Int
unWork (Work w) = w

workDelta :: Monad m => Work -> StateT Work m ()
workDelta (Work dw) = modify' (\(Work w) -> Work (w + dw))
