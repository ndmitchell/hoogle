
module Test where

import Debug.QuickCheck
import List
import Char

import Hoogle.Parser


data LineStr = LineStr String
     deriving Show


instance Arbitrary Char where
    arbitrary = oneof $ map return (spaces ++ validChars)
        where
            spaces = replicate 10 ' '
            validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "[](),->=:!"
            anyChars = map chr [0x20..0x70]

    
instance Arbitrary LineStr where
    arbitrary = do x <- vector 25
                   return $ LineStr x


prop_NoParseErrors :: LineStr -> Bool
prop_NoParseErrors (LineStr x) =
    case parser x of
         Left _ -> True
         Right _ -> True
         



test1 = quickCheck prop_NoParseErrors
