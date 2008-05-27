
module ClassesEx where


class Class1 a where
    func1 :: a -> Bool
    
class Class1 a => Class2 a where
    func2 :: a -> Bool


data Data1 = Data1

data Data2 a = Data2 a


instance Class1 Data1 where
    func1 a = True

instance Class2 Data1 where
    func2 a = False

instance Class1 (Data2 a) where
    func1 a = True

instance Eq a => Class2 (Data2 a) where
    func2 a = True


func3 :: Class1 a => a -> Bool
func3 x = func1 x

func4 :: (Eq a, Class2 a) => a -> Bool
func4 x = True

func5 :: (Class1 a, Class2 b) => a -> b -> Bool
func5 x y = True
