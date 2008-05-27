
-- basic class/instance definitions

module Classes where


data Data1 = Data1
             deriving Eq
             

instance Show Data1 where
    show x = ""


class Class1 x where
    func1 :: x -> Bool
    func2 :: x -> x -> x
    
    func3 :: x -> x -> Bool
    func3 a b = func1 a && func1 b


instance Class1 Data1 where
    func1 = error "todo"
    func2 = error "todo"

