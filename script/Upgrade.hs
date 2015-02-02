
import Data.List.Extra
import Control.Monad
import System.Directory.Extra
import Data.Time.Clock
import System.Process.Extra
import Control.Exception.Extra
import Data.Time.Clock
import Data.Time.Format
import System.FilePath
import System.Locale


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
    createDirectoryIfMissing True "hoogle-upgrade"
    dirs <- filterM doesDirectoryExist =<< listContents "hoogle-upgrade"
    forM_ (dropEnd 10 $ sort dirs) $ \dir -> do
        putStrLn $ "Cleaning " ++ dir ++ "..."
        removeDirectoryRecursive dir

    now <- getCurrentTime
    let dir = "hoogle-upgrade/" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" now
    now <- createDirectoryIfMissing True dir
    withCurrentDirectory dir $ do
        putStrLn $ "Upgrading into " ++ dir
        system_ "git clone https://github.com/ndmitchell/hoogle.git ."
        system_ "cabal update"
        system_ "cabal configure \"--ghc-options=-rtsopts -O2 -threaded\""
        system_ "cabal build"
        let exe = normalise "dist/build/hoogle/hoogle"
        system_ $ exe ++ " generate +RTS -M1G"
        system_ $ exe ++ " test"
        ignore $ system_ "pkill hoogle"
        system_ $ "nohup " ++ exe ++ " server --port=8080 --log=../../log.txt +RTS -N3 >> ../../out.txt &"
        writeFile "downgrade.sh" "pkill hoogle\nnohup dist/build/hoogle/hoogle server --port=8080 --log=../../log.txt >> ../../out.txt &\n"
    appendFile "hoogle-upgrade/upgrade.txt" $ dir ++ "\n"
    putStrLn "Successfully upgraded"
