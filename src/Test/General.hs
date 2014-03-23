
module Test.General(parseTest, (===), randCheck) where

import Control.Monad
import qualified Data.ByteString as BS
import Test.QuickCheck(Arbitrary(..), quickCheckWithResult, stdArgs, Testable, Result(..))


instance Arbitrary BS.ByteString where
    arbitrary = fmap BS.pack arbitrary


parseTest :: (Show a, Show e, Eq a) => (String -> Either e a) -> String -> a -> IO ()
parseTest f input output =
    case f input of
        Left x -> err "Parse failed" (show x)
        Right x -> when (x /= output) $ err "Parse not equal" (show x)
    where
        err pre post = error $ pre ++ ":\n  " ++ input ++ "\n  " ++ show output ++ "\n  " ++ post


(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = when (a /= b) $ error $ "Expected: " ++ show a ++ "\nGot: " ++ show b

randCheck :: Testable a => a -> IO ()
randCheck p = do
    res <- quickCheckWithResult stdArgs p
    let bad = case res of Failure{} -> True; GaveUp{} -> True; _ -> False
    when bad $ error "QuickCheck failed"
