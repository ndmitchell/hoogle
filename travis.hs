
import System.Time.Extra
import System.Process.Extra
import Control.Exception.Extra


main :: IO ()
main = do
    let timed x = do
            putStrLn $ "\n\nSearching for " ++ x
            (time,_) <- duration $ system_ $ "hoogle +RTS -T -M2G -K10M -RTS " ++ show x
            putStrLn $ "Search " ++ show x ++ " took " ++ showDuration time
            putStrLn "\n\n"
    retry 3 $ timed "generate" >> sleep 10
    timed "test"
    timed "map"
    timed "map package:base"
    timed ":: a -> b"
    timed ":: ShakeOptions -> a"
    retry 3 $ timed "generate --frege" >> sleep 10
