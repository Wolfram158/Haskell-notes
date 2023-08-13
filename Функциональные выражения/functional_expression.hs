import qualified Data.Map as Map
juxt x (f:fs) = map (\t -> (t x)) (f:fs)
constant value = \args -> value
variable name = \args -> ((Map.fromList args) Map.! name)
abstract f start = \exprs -> (\args -> (foldl f start (juxt args exprs)))
add = abstract (+) 0
multiply = abstract (*) 1
f = constant 8
g = constant 9
h = variable "x"
ss = add [f, g]
mm = multiply [ss, h]
i = variable "y"
j = variable "z"
cc = add [mm, ss, f, g, i, j]
main = do
        putStrLn (show (ss [("x", 3)]))
        putStrLn (show (mm [("x", 10)]))
        putStrLn (show (cc [("x", 5), ("y", 10), ("z", 15)]))
