
-- | Some basic tests with operators

module Operators where


(++++) :: a -> a -> Bool
a ++++ b = True


(***) :: a -> b -> Bool
a *** b = True


data Data1 a = a :|: a | Data2
