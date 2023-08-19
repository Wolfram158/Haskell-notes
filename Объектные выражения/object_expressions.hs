import qualified Data.Map as Map
data Expression = Constant Int | Variable String | Add Expression Expression | Multiply Expression Expression deriving (Show, Eq)
evaluate :: Expression -> [(String, Int)] -> Int
evaluate (Add x y) args = (evaluate x args) + (evaluate y args)
evaluate (Multiply x y) args = (evaluate x args) * (evaluate y args)
evaluate (Constant x) args = x
evaluate (Variable name) args = ((Map.fromList args) Map.! name)
a = Add (Constant 3) (Constant 4)
b = Add a (Variable "x")
c = Multiply b (Variable "y")
main = do
         putStrLn $ show $ evaluate a [("x", 3)]
         putStrLn $ show $ evaluate b [("x", 101)]
         putStrLn $ show $ evaluate c [("x", 101), ("y", 10)]
