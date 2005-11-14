{-
    This file is part of Hoogle, (c) Neil Mitchell 2004-2005
    http://www.cs.york.ac.uk/~ndm/hoogle/
    
    This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/2.0/
    or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
-}

module Hoogle.Database(
    Database(..), 
    loadDatabase
    ) where

import Data.Maybe
import Data.List

import Hoogle.TypeSig
import Hoogle.Parser
import Hoogle.Lexer
import Hoogle.General

import Hoogle.MatchName
import Hoogle.MatchType
import Hoogle.TypeAlias
import Hoogle.MatchClass


-- | An abstract data type
data Database = Database
    {
        aliases :: AliasTable,
        names :: NameTable,
        types :: TypeTable,
        classes :: ClassTable
    }
    deriving Show
    

-- | load a text file into a 'Database'
loadTextfile :: String -> Database
loadTextfile x = Database {
        aliases = buildAlias $ filter isTypeAlias items,
        names = buildName $ map ((,) []) modules ++ modnamed,
        types = buildType $ filter (isFunc . snd) modnamed,
        classes = buildClass $ filter isInstance items
        }
    where
        -- all the items in the file
        items = catLefts $ map parser $ filter validLine $ lines x
        
        (instances, namedItems) = partition isInstance items
        (modules, modnamed) = modulify namedItems
        

-- take a list of items, and return those which are modules
-- and tag every other item with its module
modulify :: [Item] -> ([Item], [(ModuleName, Item)])
modulify xs = f [] xs
    where
        f _ (Module x:xs) = (Module x:a, b)
            where (a,b) = f x xs
            
        f m (x:xs) = (a, (m,x):b)
            where (a,b) = f m xs
        
        f _ [] = ([], [])


-- | Load a database from a file
--   perform all cache'ing requried
loadDatabase :: String -> IO Database
loadDatabase file = do x <- readFile file
                       return $ loadTextfile x
