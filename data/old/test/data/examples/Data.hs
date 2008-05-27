
-- | More complex tests involving data

module Data where


data Data1 a = Data2 {func1 :: a, func2 :: Int}
             | Data3 {func3 :: Bool, func1 :: a}
             | Data4


type Type1 = Data1 Bool


newtype Data5 a = Data6 a


