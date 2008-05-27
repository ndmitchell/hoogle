{-# OPTIONS_GHC -fglasgow-exts #-}

module GhcExts where

import GHC.Exts


data Data1 = forall a. Data2 a (a -> Bool)
         | Data3
         

data Baz = forall a. Eq a => Baz1 a a
         | forall b. Show b => Baz2 b (b -> b)
         
         
class Seq s a where
    element :: Eq a => a -> s a -> Bool


class Foo a b c | a b -> c where
    none :: c -> Bool


func1 :: Int# -> Float#
func1 = error ""

func2 :: forall a b. a -> b -> a
func2 = error ""

func3 :: forall a b. (Ord a, Eq  b) => a -> b -> a
func3 = error ""

func4 :: (forall a. a->a) -> Int -> Int
func4 = error ""

func5 :: (forall a. Eq a => [a] -> a -> Bool) -> Int -> Int
func5 f = error ""

func6 :: ((forall a. a->a) -> Int) -> Bool -> Bool
func6 f = error ""

