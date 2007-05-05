
module Hoogle.DataBase.Instances(
    Instances, createInstances
    ) where

import Hoogle.Item.All

{-
Instances are a little tricky because they are so different on GHC/Hugs/Haskell.

We supports multi-parameter type classes, but only slightly. The rule is:

Class a b === (Class$0 a, Class$1 b)

Instances are stored as:

Class (Data a b) ==> (Class1 a, Class2 a, Class1 b, Class2 b)
Class * ==> True

We are able to capture Haskell 98 classes perfectly, and give a reasonable
interpretation of other things. There are two ways of encoding classes - 'or' and
'and'. Each definition is one instance of an 'or', but may be multiple 'and's.
For example:

Class (Data a Bool) is translated to:
    Class (Data a b) ==> Class_Bool b
    Class_Bool Bool ==> True
-}

type ClassName = String
type DataName  = String

data Instances = Instances (Set.Set ClassName)
                           (Map.Map (ClassName,DataName) [(ClassName,Int)])


createInstances :: [Item] -> Instances
createInstances _ = Instances Set.empty Map.empty

