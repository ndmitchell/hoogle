
import System.Time.Extra
import System.Process.Extra


main :: IO ()
main = do
    let timed x = do
        putStrLn $ "\n\nSearching for " ++ x
        (time,_) <- duration $ system_ $ "hogle +RTS -T -RTS " ++ (if x == "" then "" else show x)
        putStrLn $ "Search " ++ show x ++ " took " ++ showDuration time
        putStrLn "\n\n"
    timed "test"
    timed "generate"
    timed "map"
    timed "map package:base"
    timed ":: a -> b"
    timed ":: ShakeOptions -> a"
