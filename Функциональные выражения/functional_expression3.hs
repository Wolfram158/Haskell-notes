import qualified Data.Map as Map
juxt x fs = map (\t -> (t x)) fs
constant value = \args -> value
variable name = \args -> ((Map.fromList args) Map.! name)
abstract f g = \(e:es) -> (\args -> (foldl f (g (e args)) (map g (juxt args es))))
add = abstract (+) id
multiply = abstract (*) id
sub = abstract (-) id
sumexp = abstract (+) exp
ln = abstract (\x y -> log ((exp x) + (exp y))) log 
lse = \es -> ln [sumexp es]
divide = \(e:es) -> (\args -> (if (length es) == 0 then (1 / (e args)) else (foldl (/) (e args) (juxt args es))))
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
r = divide [add [sub [i, j], multiply [i, h]]]
main = do
        putStrLn (show (ss [("x", 3)]))
        putStrLn (show (mm [("x", 10)]))
        putStrLn (show (cc [("x", 5), ("y", 10), ("z", 15)]))
        putStrLn (show (subs [("x", 10)]))
        putStrLn (show (d [("x", 20), ("y", 5), ("z", 4)]))
        putStrLn (show (r [("x", 2), ("y", 4), ("z", 8)]))
        putStrLn (show ((sumexp [add [(variable "x"), (variable "y")]]) [("x", 1), ("y", 2)]))
        putStrLn (show ((lse [constant 3, variable "x"]) [("x", 4), ("y", 3)]))
