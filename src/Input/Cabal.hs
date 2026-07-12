{-# LANGUAGE ViewPatterns, PatternGuards, TupleSections, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

-- | Module for reading Cabal files.
module Input.Cabal(
    PkgName, Package(..),
    parseCabalTarball, readGhcPkg,
    packagePopularity, readCabal
    ) where

import Input.Settings

import Data.List.Extra
import Data.Map (Map)
import System.FilePath
import Control.DeepSeq
import Control.Exception.Extra
import Control.Monad
import System.IO.Extra
import General.Str
import System.Exit
import qualified System.Process.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Map.Strict as Map
import General.Util
import Data.Semigroup
import Control.Applicative
import Prelude

import Distribution.Compat.Lens (toListOf)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Package (packageId, UnitId, pkgName)
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import qualified Distribution.PackageDescription.Parsec as PD
import qualified Distribution.Pretty
import Distribution.Text (display)
import qualified Distribution.Types.BuildInfo.Lens as Lens
import Distribution.Types.LibraryVisibility (LibraryVisibility(..))
import Distribution.Types.PackageDescription (license')
import Distribution.Types.PackageId (pkgVersion)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.Version (Version, versionNumbers)
import Distribution.Utils.ShortText (ShortText, fromShortText)
import Hackage.RevDeps (lastVersionsOfPackages)
import qualified Distribution.SPDX as SPDX

---------------------------------------------------------------------
-- DATA TYPE

-- | A representation of a Cabal package.
data Package = Package
    {packageTags :: ![(Str, Str)] -- ^ The Tag information, e.g. (category,Development) (author,Neil Mitchell).
    ,packageLibrary :: !Bool -- ^ True if the package provides a library (False if it is only an executable with no API)
    ,packageSynopsis :: !Str -- ^ The synposis, grabbed from the top section.
    ,packageVersion :: !Str -- ^ The version, grabbed from the top section.
    ,packageDepends :: ![PkgName] -- ^ The list of packages that this package directly depends on.
    ,packageDocs :: !(Maybe FilePath) -- ^ Directory where the documentation is located
    } deriving Show

instance Semigroup Package where
    Package x1 x2 x3 x4 x5 x6 <> Package y1 y2 y3 y4 y5 y6 =
        Package (x1++y1) (x2||y2) (one x3 y3) (one x4 y4) (nubOrd $ x5 ++ y5) (x6 `mplus` y6)
        where one a b = if strNull a then b else a

instance Monoid Package where
    mempty = Package [] True mempty mempty [] Nothing
    mappend = (<>)

instance NFData Package where
    rnf (Package a b c d e f) = rnf (a,b,c,d,e,f)


---------------------------------------------------------------------
-- POPULARITY

-- | Given a set of packages, return the popularity of each package, along with any warnings
--   about packages imported but not found.

packagePopularity :: Map.Map PkgName Package -> ([String], Map.Map PkgName Int)
packagePopularity cbl = mp `seq` (errs, mp)
    where
        mp = Map.map length good
        errs =  [ unPackageName user ++ ".cabal: Import of non-existant package " ++ unPackageName name ++
                          (if null rest then "" else ", also imported by " ++ show (length rest) ++ " others")
                | (name, user:rest) <- Map.toList bad]
        (good, bad)  = Map.partitionWithKey (\k _ -> k `Map.member` cbl) $
            Map.fromListWith (++) [(b,[a]) | (a,bs) <- Map.toList cbl, b <- packageDepends bs]


---------------------------------------------------------------------
-- READERS

-- | Run 'ghc-pkg' and get a list of packages which are installed.
readGhcPkg :: Settings -> IO (Map.Map PkgName Package)
readGhcPkg settings = do
    (exit, stdout, stderr) <-
    -- From GHC 9.0.1, the `haddock-html` field in `*.conf` files for GHC boot
    -- libraries has used `${pkgroot}`, which can be expanded in the output.

    -- On Windows, the `haddock-html` field in `*.conf` files for GHC boot
    -- libraries for GHC >= 9.0 && < 9.10 contain errors. For example, this may
    -- be specified:
    --
    --     haddock-html: ${pkgroot}/../../doc/html/libraries/base-4.18.0.0
    --
    -- when the correct specification would be:
    --
    --     haddock-html: ${pkgroot}/../doc/html/libraries/base-4.18.0.0
    --
    -- However haddock does not seek to correct that. It assumes that users will
    -- correct manually the affected `*.conf` files.

    -- important to use BS process reading so it's in Binary format, see #194
      BS.readProcessWithExitCode "ghc-pkg" ["dump", "--expand-pkgroot"] mempty
    when (exit /= ExitSuccess) $
        errorIO $ "Error when reading from ghc-pkg, " ++ show exit ++ "\n" ++ UTF8.toString stderr

    installedPackages <- parsePackages stdout

    pure $
        Map.fromList
            [ ( pkgName $ packageId installedPackage
              , fromInstalledPackage settings installedPackages installedPackage
              )
            | (_unitId, installedPackage) <- Map.toList installedPackages
            ]
    where
        parsePackages :: UTF8.ByteString -> IO (Map UnitId IPI.InstalledPackageInfo)
        parsePackages input =
            Map.fromList . fmap ((,) <$> IPI.installedUnitId <*> id) . catMaybes <$>
            traverse
                (\input ->
                    case IPI.parseInstalledPackageInfo . bstrPack $ unlines input of
                        Left errors -> do
                            mapM_ (\msg -> putStrLn $ "error (parsing ghc-pkg output): " ++ msg) errors
                            pure Nothing
                        Right (warnings, package) -> do
                            mapM_ (\msg -> putStrLn $ "warning (parsing ghc-pkg output): " ++ msg) warnings
                            pure $ Just package
                )
                (splitOn ["---"] . lines . filter (/= '\r') $ UTF8.toString input)

fromInstalledPackage ::
    Settings ->
    Map UnitId IPI.InstalledPackageInfo ->
    IPI.InstalledPackageInfo ->
    Package
fromInstalledPackage settings installedPackages ipi = Package{..}
    where
        pkgId = packageId ipi

        packageDepends =
            fmap
                (\unitId ->
                    maybe
                        (error $ display unitId ++ " missing from installed packages") (pkgName . packageId)
                        (Map.lookup unitId installedPackages)
                )
                (IPI.depends ipi)
        packageVersion = mkPackageVersion $ pkgVersion pkgId
        packageSynopsis = strPack $ fromShortText $ IPI.synopsis ipi
        packageLibrary = IPI.libVisibility ipi == LibraryVisibilityPublic
        packageDocs = listToMaybe $ IPI.haddockHTMLs ipi

        packageLicenses = mkPackageLicenses . license' $ IPI.license ipi
        packageCategories = mkPackageCategories $ IPI.category ipi
        packageAuthor = fromShortText $ IPI.author ipi
        packageMaintainer = fromShortText $ IPI.maintainer ipi

        packageTags = mkPackageTags settings packageLicenses packageCategories [packageAuthor, packageMaintainer]

-- | Given a tarball of Cabal files, parse the latest version of each package.
parseCabalTarball :: Settings -> FilePath -> IO (Map.Map PkgName Package)
parseCabalTarball settings tarfile = do
    lastVersions <- lastVersionsOfPackages (const True) tarfile Nothing
    pure $ Map.map (readCabal settings) lastVersions


---------------------------------------------------------------------
-- PARSERS

readCabal :: Settings -> BStr -> Package
readCabal settings src = case PD.parseGenericPackageDescriptionMaybe src of
    Nothing -> Package
        { packageTags = []
        , packageLibrary = False
        , packageSynopsis = mempty
        , packageVersion = strPack "0.0"
        , packageDepends = []
        , packageDocs = Nothing
        }
    Just gpd -> readCabal' settings gpd

readCabal' :: Settings -> PD.GenericPackageDescription -> Package
readCabal' settings gpd = Package{..}
    where
        pd = PD.flattenPackageDescription gpd
        pkgId = PD.package pd

        packageDepends = nubOrd $ foldMap (map (\(PD.Dependency pkg _ _) -> pkg) . PD.targetBuildDepends) $ toListOf Lens.traverseBuildInfos gpd
        packageVersion = mkPackageVersion $ PD.pkgVersion pkgId
        packageSynopsis = strPack $ fromShortText $ PD.synopsis pd
        packageLibrary = PD.hasPublicLib pd
        packageDocs = Nothing

        packageLicenses = mkPackageLicenses $ PD.license pd
        packageCategories = mkPackageCategories $ PD.category pd
        packageAuthor = fromShortText $ PD.author pd
        packageMaintainer = fromShortText $ PD.maintainer pd

        packageTags = mkPackageTags settings packageLicenses packageCategories [packageAuthor, packageMaintainer]

mkPackageVersion :: Version -> Str
mkPackageVersion = strPack . intercalate "." . map show . versionNumbers

mkPackageLicenses :: SPDX.License -> [String]
mkPackageLicenses license =
    case license of
            SPDX.NONE -> []
            SPDX.License licExpr -> map (show . Distribution.Pretty.pretty) $
                unpackLicenseExpression licExpr
    where
        unpackLicenseExpression (SPDX.EOr x y) = unpackLicenseExpression x ++ unpackLicenseExpression y
        unpackLicenseExpression x = [x]

mkPackageCategories :: ShortText -> [String]
mkPackageCategories = filter (not . null) . split (`elem` " ,") . fromShortText

mkPackageTags ::
    Settings ->
    -- | Licenses
    [String] ->
    -- | Categories
    [String] ->
    -- | Authors
    [String] ->
    [(Str, Str)]
mkPackageTags settings licenses categories authors =
    map (both strPack) $ nubOrd $ concat
        [ map ("license",) licenses
        , map ("category",) categories
        , map ("author",) (concatMap (cleanup settings) authors)
        ]

-- split on things like "," "&" "and", then throw away email addresses, replace spaces with "-" and rename
cleanup :: Settings -> String -> [String]
cleanup Settings{..} =
    filter (/= "") .
    map (renameTag . intercalate "-" . filter ('@' `notElem`) . words . takeWhile (`notElem` "<(")) .
    concatMap (map unwords . split (== "and") . words) . split (`elem` ",&")
