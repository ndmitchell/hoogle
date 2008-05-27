
-- | Some basic examples

module Basic where


func1 :: a -> a
func1 x = x


func2 :: Bool -> Int
func2 True  = 1
func2 False = 0


func3 :: [a] -> [a]
func3 x = reverse x


func4 :: (a -> b) -> [a] -> [b]
func4 f x = map f x


data Data1 a = Data2 | Data3 a


data Data4 = Data5 Int Bool
