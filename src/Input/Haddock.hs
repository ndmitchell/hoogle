{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, OverloadedStrings, Rank2Types, DeriveDataTypeable, LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Input.Haddock(parseHoogle, fakePackage, input_haddock_test) where

import Language.Haskell.Exts as HSE
import Data.Char
import Data.List.Extra
import Data.Maybe
import Data.Data
import Input.Item
import Input.ParseDecl
import General.Util
import Control.DeepSeq
import Control.Monad.Trans.Class
import General.Conduit
import Control.Monad.Extra
import Control.Exception.Extra
import General.Str
import Safe
import Distribution.Types.PackageName (unPackageName, mkPackageName)


-- | An entry in the Hoogle DB
data Entry = EPackage PkgName
           | EModule ModName
           | EDecl (Decl ())
             deriving (Data, Show)


fakePackage :: PkgName -> String -> (Maybe Target, [Item])
fakePackage name desc = (Just $ Target (hackagePackageURL name) Nothing Nothing "package" (renderPackage name) desc, [IPackage name])

-- | Given a file name (for errors), feed in lines to the conduit and emit either errors or items
parseHoogle :: Monad m => (String -> m ()) -> URL -> LBStr -> ConduitM i (Maybe Target, [Item]) m ()
parseHoogle warning url body = sourceLStr body .| linesCR .| zipFromC 1 .| parserC warning .| hierarchyC url .| mapC (\x -> rnf x `seq` x)

parserC :: Monad m => (String -> m ()) -> ConduitM (Int, BStr) (Target, Entry) m ()
parserC warning = f [] ""
    where
        f com url = do
            x <- await
            whenJust x $ \(i,s) -> case () of
                _ | s == "}" -> f [] ""
                  -- Skip default methods like ($dmliftEq) and ($dmdisplayExceptionAnnotation)
                  | Just{} <- bstrStripPrefix "($dm" s -> f [] ""
                  | Just s <- bstrStripPrefix "-- | " s -> f [ignoreMath s] url
                  | Just s <- bstrStripPrefix "--" s -> f (if null com then [] else bstrTrimStart s : com) url
                  | Just s <- bstrStripPrefix "    --" s -> f (if null com then [] else bstrTrimStart s : com) url
                  | Just s <- bstrStripPrefix "@url " s -> f com (bstrUnpack s)
                  | bstrNull $ bstrTrimStart s -> f [] ""
                  | otherwise -> do
                        case parseLine $ fixLine $ bstrUnpack s of
                            Left y -> lift $ warning $ show i ++ ":" ++ y
                            -- only check Nothing as some items (e.g. "instance () :> Foo a")
                            -- don't roundtrip but do come out equivalent
                            Right [EDecl InfixDecl{}] -> pure () -- can ignore infix constructors
                            Right xs -> forM_ xs $ \x ->
                                yield (Target url Nothing Nothing (typeItem x) (renderItem x) $ reformat $ reverse com, x) -- descendBi stringShare x)
                        f [] ""


-- See https://github.com/ndmitchell/hoogle/issues/353
-- for functions like `tail` which start <math>.
ignoreMath :: BStr -> BStr
ignoreMath x | Just x <- "&lt;math&gt;" `bstrStripPrefix` x
             = fromMaybe x $ ". " `bstrStripPrefix` x
ignoreMath x = x

typeItem :: Entry -> String
typeItem = \case
    EPackage{} -> "package"
    EModule{} -> "module"
    EDecl{} -> ""

-- FIXME: used to be in two different modules, now does and then undoes lots of stuff
reformat :: [BStr] -> String
reformat = unlines . map bstrUnpack


hierarchyC :: Monad m => URL -> ConduitM (Target, Entry) (Maybe Target, [Item]) m ()
hierarchyC packageUrl = void $ mapAccumC f (Nothing, Nothing)
    where
        f (_pkg, _mod) (t, EPackage x) = ((Just (unPackageName x, url), Nothing), (Just t{targetURL=url}, [IPackage x]))
            where url = targetURL t `orIfNull` packageUrl
        f (pkg, _mod) (t, EModule x) = ((pkg, Just (strUnpack x, url)), (Just t{targetPackage=pkg, targetURL=url}, [IModule x]))
            where url = targetURL t `orIfNull` (if isGhc then ghcModuleURL x else hackageModuleURL x)
        f (pkg, mod) (_t, EDecl i@InstDecl{}) = ((pkg, mod), (Nothing, hseToItem_ i))
        f (pkg, mod) (t, EDecl x) = ((pkg, mod), (Just t{targetPackage=pkg, targetModule=mod, targetURL=url}, hseToItem_ x))
            where url = targetURL t `orIfNull` case x of
                            _ | [n] <- declNames x -> hackageDeclURL (isTypeSig x) n
                              | otherwise -> ""

        isGhc = "~ghc" `isInfixOf` packageUrl || "/" `isSuffixOf` packageUrl

        hseToItem_ x = hseToItem x `orIfNull` error ("hseToItem failed, " ++ pretty x)
        infix 1 `orIfNull`
        orIfNull x y = if null x then y else x

renderPackage :: PkgName -> [Char]
renderPackage x = "<b>package</b> <span class=name><s0>" ++ escapeHTML (unPackageName x) ++ "</s0></span>"

renderModule :: Str -> [Char]
renderModule (breakEnd (== '.') . strUnpack -> (pre,post)) = "<b>module</b> " ++ escapeHTML pre ++ "<span class=name><s0>" ++ escapeHTML post ++ "</s0></span>"


renderItem :: Entry -> String
renderItem = keyword . focus
    where
        keyword x | Just b <- stripPrefix "type family " x = "<b>type family</b> " ++ b
                  | (a,b) <- word1 x, a `elem` kws = "<b>" ++ a ++ "</b> " ++ b
                  | otherwise = x
            where kws = words "class data type newtype"

        name x = "<span class=name>" ++ x ++ "</span>" :: String

        focus (EModule x) = renderModule x
        focus (EPackage x) = renderPackage x
        focus (EDecl x) | [now] <- declNames x, (pre,stripPrefix now -> Just post) <- breakOn now $ pretty x =
            if "(" `isSuffixOf` pre && ")" `isPrefixOf` post then
                init (escapeHTML pre) ++ name ("(" ++ highlight now ++ ")") ++ escapeHTML (tailErr post)
            else
                escapeHTML pre ++ name (highlight now) ++ escapeHTML post
        focus (EDecl x) = pretty x

        highlight :: String -> String
        highlight x = "<s0>" ++ escapeHTML x ++ "</s0>"


parseLine :: String -> Either String [Entry]
parseLine x@('@':str) = case a of
        "package" | [b] <- words b, b /= "" -> Right [EPackage $ mkPackageName b]
        "version" -> Right []
        _ -> Left $ "unknown attribute: " ++ x
    where (a,b) = word1 str
parseLine (stripPrefix "module " -> Just x) = Right [EModule $ strPack x]
parseLine x | Just x <- readItem x = case x of
    TypeSig a bs c -> Right [EDecl (TypeSig a [b] c) | b <- bs]
    x -> Right [EDecl x]
parseLine x = Left $ "failed to parse: " ++ x


fixLine :: String -> String
fixLine (stripPrefix "instance [incoherent] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlap ok] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [overlapping] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "instance [safe] " -> Just x) = fixLine $ "instance " ++ x
fixLine (stripPrefix "(#) " -> Just x) = "( # ) " ++ x
fixLine ('[':x:xs) | isAlpha x || x `elem` ("_(" :: String), (a,']':b) <- break (== ']') xs = x : a ++ b
fixLine ('[':':':xs) | (a,']':b) <- break (== ']') xs = "(:" ++ a ++ ")" ++ b
-- Record field accessor can start from '$', such as ($*) in algebra-4.3.1
fixLine ('[':'$':xs) | (a,']':b) <- break (== ']') xs = "($" ++ a ++ ")" ++ b
-- Record field accessor can start from '>', such as (>>-) in turtle-1.6.2
fixLine ('[':'>':xs) | (a,']':b) <- break (== ']') xs = "(>" ++ a ++ ")" ++ b
fixLine x | "class " `isPrefixOf` x = fst $ breakOn " where " x
fixLine x = x


readItem :: String -> Maybe (Decl ())
readItem x -- newtype
    | Just x <- stripPrefix "newtype " x
    , ParseOk (DataDecl an _ b c d e) <- fmap unGADT $ myParseDecl $ "data " ++ x
    = Just $ DataDecl an (NewType ()) b c d e
readItem x@(x0 : _) -- constructors
    | isUpper x0 || x0 == '('
    , ParseOk (GDataDecl _ _ _ _ _ [GadtDecl s name _ _ _ ty] _) <- myParseDecl $ "data Data where " ++ x
    , let f (TyBang _ _ _ (TyParen _ x@TyApp{})) = x
          f (TyParen _ x@TyApp{}) = x
          f (TyBang _ _ _ x) = x
          f x = x
    = Just $ TypeSig s [name] $ applyFun1 $ map f $ unapplyFun ty
readItem x | ParseOk y <- myParseDecl x = Just $ unGADT y
readItem _ = Nothing

unGADT :: Decl l -> Decl l
unGADT (GDataDecl a b c d _  [] e) = DataDecl a b c d [] e
unGADT x = x

prettyItem :: Entry -> String
prettyItem (EPackage x) = "package " ++ unPackageName x
prettyItem (EModule x) = "module " ++ strUnpack x
prettyItem (EDecl x) = pretty x


input_haddock_test :: IO ()
input_haddock_test = testing "Input.Haddock.parseLine" $ do
    let a === b | fmap (map prettyItem) (parseLine a) == Right [b] = putChar '.'
                | otherwise = errorIO $ show (a,b,parseLine a, fmap (map prettyItem) $ parseLine a)
    let test a = a === a
    test "type FilePath = [Char]"
    test "data Maybe a"
    test "Nothing :: Maybe a"
    test "Just :: a -> Maybe a"
    test "newtype Identity a"
    test "foo :: Int# -> b"
    test "(,,) :: a -> b -> c -> (a, b, c)"
    "data (,,) a b" === "data Tuple3 a b" -- when ghc-lib-parser >= 9.8
    test "reverse :: [a] -> [a]"
    -- Parallel Haskell has never been implemented
    -- test "reverse :: [:a:] -> [:a:]"
    test "module Foo.Bar"
    test "data Char"
    "data Char :: *" === "data Char"
    "newtype ModuleName :: *" === "newtype ModuleName"
    "Progress :: !(Maybe String) -> {-# UNPACK #-} !Int -> !(Int -> Bool) -> Progress" ===
        "Progress :: Maybe String -> Int -> (Int -> Bool) -> Progress"
    test "quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)"
    test "( # ) :: Int"
    test "pattern MyPattern :: ()"
    test "degrees :: Floating x => Radians x -> Degrees x"
    test "class Angle a"
    test "instance Eq x => Eq (Degrees x)"
    test "instance Angle Degrees"
    test "type Queue a = Deque Nonthreadsafe Nonthreadsafe SingleEnd SingleEnd Grow Safe a"
    test "class DequeClass d => PopL d"
    test "tests_fifo :: DequeClass d => (forall elt . IO (d elt)) -> Test"
    test "class ParUnsafe iv p | p -> iv"
    test "(##) :: Diagram -> Diagram -> Diagram"
    test "instance LayoutClass Positioned []"
    test "data Ord a => Range a"
    test "aPair :: Proxy (,)"
    test "aTriple :: Proxy (,,)"
    test "qop :: (Ord a, Show qtyp, Show (QFlipTyp qtyp), QFlipTyp (QFlipTyp qtyp) ~ qtyp) => Set (QueryRep QAtomTyp a) -> Set (QueryRep (QFlipTyp qtyp) a) -> QueryRep qtyp a"
    test "reorient :: (Unbox a) => Bernsteinp Int a -> Bernsteinp Int a"
    "type family PrimM a :: * -> *;" === "type family PrimM a :: * -> *"
    test "HSNil :: HSet '[]"
    "HSCons :: !elem -> HSet elems -> HSet (elem : elems)" === "HSCons :: elem -> HSet elems -> HSet (elem : elems)"
    test "instance Data.HSet.Reverse.HReverse '[e] els1 els2 => Data.HSet.Reverse.HReverse '[] (e : els1) els2"
    test "instance Data.HSet.Remove.HRemove (e : els) els 'TypeFun.Data.Peano.Z"
    test "Free :: (forall m . Monad m => Effects effects m -> m a) -> Free effects a"
    test "infixl 3 <||"
    test "instance Data.String.IsString t => Data.String.IsString (t Yi.MiniBuffer.::: doc)"
    test "runValueExpression :: (Functor f) => Expression a ((->) b) f r -> f ((a -> b) -> r)"
    test "HCons :: (x :: *) -> HList xs -> HList (x : xs)"
    test "instance forall k (key :: k) . Data.Traversable.Traversable (Data.ComposableAssociation.Association key)"
    test "ReflH :: forall (k :: *) (t :: k) . HetEq t t"
    test "egcd :: (PID d, (Euclidean d)) => d -> d -> (d, d, d)"
    test "proc :: FilePath -> [String] -> CreateProcess"
    test "unitTests :: Proxy '()"
    test "type OneToFour = '[1, 2, 3, 4]"
    test "data family Prio pol item :: *"
    test "set :: (Monad m, ToByteString a) => Key -> a -> Opts \"SET\" -> Redis m Bool"
    test "by :: ByteString -> Opts \"SORT\""
    test "infixr 9 :+:"
    test "instance forall k1 k2 (expectation1 :: k2) (expectation2 :: k1) . (Test.TypeSpec.Core.PrettyTypeSpec expectation1, Test.TypeSpec.Core.PrettyTypeSpec expectation2) => Test.TypeSpec.Core.PrettyTypeSpec '(expectation1, expectation2)"
    test "SomeFoo :: Foo a => m a -> SomeFoo m"
    test "(@~?) :: (HasCallStack, Ord a, Num a, Show a, ?epsilon :: a) => a -> a -> Assertion"
    test "data Data where { Idx :: {idxChildren :: Index key (Node height key val)} -> Node ('S height) key val}"
    test "UnexpectedResponse :: forall k a b . () => Host -> Response k a b -> ProtocolError"
    test "(.) :: Category k cat => forall (b :: k) (c :: k) (a :: k) . cat b c -> cat a b -> cat a c"
    test "infixl 3 `And`"
    test "infix 1 `shouldBe`"
    test "pattern The :: The d a => a -> d"
    test "Html :: Element \"html\" '[] (Elements [\"head\", \"body\"]) (ManifestA & '[])"
    test "instance forall k1 v1 (pk :: k1 -> GHC.Types.Constraint) (k2 :: k1) (pv :: v1 -> GHC.Types.Constraint) (v2 :: v1) . (pk k2, pv v2) => Type.Membership.KeyTargetAre pk pv (k2 'Type.Membership.Internal.:> v2)"
    -- The following no longer parses since GHC 9.14 (which is the correct behaviour)
    -- test "crDoubleBuffer :: CompactorReturn s -> {-# UNPACK #-} !DoubleBuffer s"
    test "expectationFailure :: (?callStack :: CallStack) => String -> Expectation"
    test "type family MapTyCon t xs = r | r -> xs"
    test "pattern Id :: CRCategory k => (β ~ α, Object k α) => k α β"
    test "pattern Stream :: () => () => Repetition"
    test "In# :: (# #) -> In (a :: Effects) (b :: Effects)"
    test "anyAsciiDecimalWord# :: Addr# -> Addr# -> (# (# #) | (# Word#, Addr# #) #)"
    test "class SymbolToField (sym :: Symbol) rec typ | sym rec -> typ"
    test "closestPairDist_spec :: _ => ([r] -> r) -> (r -> t) -> [b] -> Property"
    -- Cannot faithfully represent ConstraintKind with ImplicitParams in HSE
    -- test "type HasCallStack = ?callStack :: CallStack"
    -- Cannot faithfully represent @r in HSE
    -- test "Maybe# :: forall (r :: RuntimeRep) (a :: TYPE r). (# (# #) | a #) -> Maybe# @r a"
    -- Cannot faithfully represent visible binders in HSE
    -- test "data NDFamily_ :: forall (name :: Name) -> forall (ks :: Params name). ParamsProxy name ks -> Res name ks Any :~: r -> Args name ks -> Exp r"
    -- Cannot faithfully represent standalone kind signatures in HSE
    -- test "type MinBound :: a;"
