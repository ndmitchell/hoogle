{-# LANGUAGE PatternGuards #-}

module General.Util(
    fileSize,
    pretty,
    fromName, fromTyVarBind,
    tarballReadFiles,
    template,
    escapeHTML
    ) where

import System.IO
import Language.Haskell.Exts
import Data.List.Extra
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative
import Codec.Compression.GZip as GZip
import Codec.Archive.Tar as Tar


fileSize :: FilePath -> IO Int
fileSize file = withFile file ReadMode $ fmap fromIntegral . hFileSize

pretty :: Pretty a => a -> String
pretty = trim . unwords . words . prettyPrint

fromName :: Name -> String
fromName (Ident x) = x
fromName (Symbol x) = x

fromTyVarBind :: TyVarBind -> Name
fromTyVarBind (KindedVar x _) = x
fromTyVarBind (UnkindedVar x) = x


tarballReadFiles :: FilePath -> IO [(FilePath, LBS.ByteString)]
tarballReadFiles file = f . Tar.read . GZip.decompress <$> LBS.readFile file
    where
        f (Next e rest) | NormalFile body _ <- entryContent e = (entryPath e, body) : f rest
        f (Next _ rest) = f rest
        f Done = []
        f (Fail e) = error $ "tarballReadFiles on " ++ file ++ ", " ++ show e


template :: [(String, String)] -> String -> String
template vars = f
    where
        f ('#':'{':xs) | (name,'}':rest) <- break (== '}') xs, Just val <- lookup name vars = val ++ f rest
        f (x:xs) = x : f xs
        f [] = []


-- | Take a piece of text and escape all the HTML special bits
escapeHTML :: String -> String
escapeHTML = concatMap f
    where
        f '<' = "&lt;"
        f '>' = "&gt;"
        f '&' = "&amp;"
        f '\"' = "&quot;"
        f  x  = [x]
