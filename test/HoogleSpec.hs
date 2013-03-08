{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Filesystem (isDirectory, isFile)
import qualified Hoogle as H
import           System.Environment
import           System.Exit
import           System.IO
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

main :: IO ()
main = do
    env <- getEnvironment
    hoogledb <- liftIO $
        readFileUtf8 "datadir/testdata.txt"
            >>= return . snd . H.createDatabase H.Haskell []
    hspec $ hoogleSpec hoogledb

readFileUtf8 x = do
    h <- openFile x ReadMode
    hSetEncoding h utf8
    hGetContents h

hoogleSpec :: H.Database -> Spec
hoogleSpec db = do
    describe "Basic functionality" $ do
        it "finds 'snd'" $ do
            let q = H.parseQuery undefined "snd"
            map (H.self . snd) (H.search db (either mempty id q))
                @?= [H.Tags [ H.TagBold (H.Tags [H.TagEmph (H.Str "snd")])
                            , H.Str " :: "
                            , H.Str "(a,b)"
                            , H.Str " -> "
                            , H.Str "b" ]]

        it "finds four instances of 'fst'" $ do
            let q = H.parseQuery undefined "fst"
            length (H.search db (either mempty id q)) @?= 4

        it "finds two 'Foo.Bar.fst*' inexactly" $ do
            let q = H.parseQuery undefined "fst +Foo.Bar"
            length (H.search db (either mempty id q)) @?= 2

        it "finds 'Foo.Bar.fst' exactly" $ do
            let q = H.parseQuery undefined "fst +Foo.Bar"
            map (H.self . snd)
                (H.search db (H.queryExact (Just H.UnclassifiedItem)
                                           (either mempty id q)))
                @?= [H.Tags [ H.TagBold (H.Tags [H.TagEmph (H.Str "fst")])
                            , H.Str " :: "
                            , H.Str "(Unit,Unit)"
                            , H.Str " -> "
                            , H.Str "Unit" ]]

-- Smoke.hs ends here
