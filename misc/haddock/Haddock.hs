{-# LANGUAGE TypeOperators, IncoherentInstances #-}

module Haddock where


-- | BUG 1: bug1 will not have any documentation
class  BUG1 a  where
    -- | This documentation is dropped
    bug1 :: Integer -> a


data a :**: b = Bug2 -- ^ BUG 2: The :**: is prefix without brackets


data Bug3 = Bug3 {-# UNPACK #-} !Int -- ^ FIXED 3 (Haddock 2.8): This Int gets two !!'s before it


-- | BUG 4: The instance below has [incoherent] on it
bug4 :: (); bug4 = ()
instance Num ()
