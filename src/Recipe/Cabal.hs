
module Recipe.Cabal(
    Cabal(..), readCabal
    ) where

import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version
import Recipe.Haddock
import Recipe.Warning
import Control.Exception


ghcVersion = [7,4,1]

data Cabal = Cabal
    {cabalName :: String
    ,cabalVersion :: String
    ,cabalDescription :: [String]
    ,cabalDepends :: [String]
    } deriving Show


readCabal :: FilePath -> IO (Maybe Cabal)
readCabal file = handle (\e -> do
    putWarning $ "Failure when reading " ++ file ++ ", " ++ show (e :: SomeException)
    return Nothing) $ fmap Just $ do
        pkg <- readPackageDescription silent file
        let plat = Platform I386 Linux
            comp = CompilerId GHC (Version ghcVersion [])
        pkg <- return $ case finalizePackageDescription [] (const True) plat comp [] pkg of
            Left _ -> flattenPackageDescription pkg
            Right (pkg,_) -> pkg
        return $ Cabal
            (display $ pkgName $ package pkg)
            (display $ pkgVersion $ package pkg)
            (haddockToHTML $ description pkg)
            [display x | Just l <- [library pkg], Dependency x _ <- targetBuildDepends $ libBuildInfo l]
