-- Script to upgrade a copy of Hoogle, run every day on haskell.org.
-- See docs/Haskell.org.md for details.

import Data.List.Extra
import Control.Monad
import Control.Applicative
import System.Directory.Extra
import System.Environment
import Data.Time.Clock
import System.Process.Extra
import Control.Exception.Extra
import Data.Time.Clock
import Data.Time.Format
import System.FilePath


-- APPROACH
-- have a directory "hoogle-upgrade" in which you do your work
-- inside each directory is a datetime name, e.g. hoogle-upgrade/20150126T001005
-- first delete all directories that are not the most recent 10
-- then create the new directory, git pull, cabal update, build, generate packages, test
-- kill the old server, start the new one
-- add yourself to a list of who was successfully started
-- write out a file in your directory called downgrade.sh, to do a quick downgrade if necessary
main :: IO ()
main = do
    args <- getArgs
    let new = "--new" `elem` args -- set on the new Hoogle server setup

    createDirectoryIfMissing True "hoogle-upgrade"

    now <- getCurrentTime
    let dir = "hoogle-upgrade/" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" now
    now <- createDirectoryIfMissing True dir
    withCurrentDirectory dir $ do

        -- Compile source
        putStrLn $ "Upgrading into " ++ dir
        echo system_ "git clone https://github.com/ndmitchell/hoogle.git ."
        sha1 <- trim <$> echo systemOutput_ "git rev-parse HEAD"
        echo system_ "cabal update"
        echo system_ "cabal install --only-dependencies --upgrade-dependencies --force-reinstalls --ghc-options=\"+RTS -M700M\""
        echo system_ "cabal configure \"--ghc-options=-rtsopts -O2\""
        echo system_ "GHCRTS=-M700M cabal build"
        let exe = normalise "dist/build/hoogle/hoogle"

        -- Compile databases
        echo system_ $ "hoogle_datadir=. " ++ exe ++ " generate --database=haskell.hoo +RTS -M900M -T -N2"
        echo system_ $ "hoogle_datadir=. " ++ exe ++ " test --database=haskell.hoo"

        echo system_ $ "hoogle_datadir=. " ++ exe ++ " generate --database=frege.hoo --frege +RTS -M900M -T -N2"

        createDirectoryIfMissing True "daml"
        echo system_ "curl https://docs.daml.com/hoogle_db/base.txt --output daml/base.txt"
        echo system_ $ "hoogle_datadir=. " ++ exe ++ " generate --database=daml.hoo --local=daml +RTS -M900M -T -N2"

        ignore $ echo system_ "pkill hoogle"
        let hoogle database port log links = echo system_ $
                "hoogle_datadir=. " ++
                "nohup " ++ exe ++ " server --database=" ++ database ++ " " ++
                "--port=" ++ show (if new then 50021 + port else 8443 + port) ++ " " ++
                (if new then "" else "--https --key=/etc/letsencrypt/live/hoogle.haskell.org/privkey.pem --cert=/etc/letsencrypt/live/hoogle.haskell.org/fullchain.pem ") ++
                "--cdn=//rawcdn.githack.com/ndmitchell/hoogle/" ++ sha1 ++ "/html/ " ++
                "--log=../../log" ++ log ++ ".txt " ++ (if links then "--links " else "") ++ " +RTS -T -N4 >> ../../out" ++ log ++ ".txt 2>&1 &"
        hoogle "haskell.hoo" 0 "" True
        hoogle "frege.hoo" 1 "-frege" False
        hoogle "daml.hoo" 2 "-daml" False

        unless new $ do
            ignore $ echo system_ "pkill rdr2tls"
            echo system_ "nohup rdr2tls --port=8080 --path=hoogle.haskell.org >> ../../out-rdr2tls.txt 2>&1 &"
            echo system_ "nohup rdr2tls --port=8081 --path=hoogle.haskell.org:8444 >> ../../out-frege-rdr2tls.txt 2>&1 &"
            echo system_ "nohup rdr2tls --port=8082 --path=hoogle.haskell.org:8445 >> ../../out-daml-rdr2tls.txt 2>&1 &"

    appendFile "hoogle-upgrade/upgrade.txt" $ dir ++ "\n"

    dirs <- filterM doesDirectoryExist =<< listContents "hoogle-upgrade"
    forM_ (dropEnd 10 $ sort dirs) $ \dir -> do
        putStrLn $ "Cleaning " ++ dir ++ "..."
        removeDirectoryRecursive dir

    putStrLn "Successfully upgraded"


echo :: (String -> IO a) -> String -> IO a
echo f x = putStrLn ("# " ++ x) >> f x
