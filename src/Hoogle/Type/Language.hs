{-# LANGUAGE DeriveDataTypeable #-}

module Hoogle.Type.Language where

import General.Base


-- | The languages supported by Hoogle.
data Language
    = Haskell -- ^ The Haskell language (<http://haskell.org/>), along with many GHC specific extensions.
    deriving (Enum,Read,Show,Eq,Ord,Bounded,Data,Typeable)
