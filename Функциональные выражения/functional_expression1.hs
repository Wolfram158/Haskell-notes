import qualified Data.Map as Map
juxt x (f:fs) = map (\t -> (t x)) (f:fs)
constant value = \args -> value
variable name = \args -> ((Map.fromList args) Map.! name)
abstract f = \(e:es) -> (\args -> (foldl f (e args) (juxt args es)))
add = abstract (+)
multiply = abstract (*)
sub = abstract (-)
divide = abstract (/)
f = constant 8
g = constant 9
h = variable "x"
ss = add [f, g]
mm = multiply [ss, h]
i = variable "y"
j = variable "z"
cc = add [mm, ss, f, g, i, j]
subs = sub [f, g]
d = divide [mm, subs, i, j]
main = do
        putStrLn (show (ss [("x", 3)]))
        putStrLn (show (mm [("x", 10)]))
        putStrLn (show (cc [("x", 5), ("y", 10), ("z", 15)]))
        putStrLn (show (subs [("x", 10)]))
        putStrLn (show (d [("x", 20), ("y", 5), ("z", 4)]))
