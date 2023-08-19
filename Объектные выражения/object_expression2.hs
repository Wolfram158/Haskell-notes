import qualified Data.Map as Map
data Expression = Constant Int | Variable String | Add Expression Expression | Multiply Expression Expression
instance Show Expression where
  show (Constant value) = show value
  show (Variable name) = name
  show (Add (Constant x) (Constant y)) = show (x + y) // ------> toMiniString
  show (Add x y) = mconcat ["(", (show x), "+", (show y), ")"]
  show (Multiply x y) = mconcat [(show x), "*", (show y)]
evaluate :: Expression -> [(String, Int)] -> Int
evaluate (Add x y) args = (evaluate x args) + (evaluate y args)
evaluate (Multiply x y) args = (evaluate x args) * (evaluate y args)
evaluate (Constant value) args = value
evaluate (Variable name) args = ((Map.fromList args) Map.! name)
diff :: Expression -> String -> Expression
diff (Add x y) name = Add (diff x name) (diff y name)
diff (Constant value) name = Constant 0
diff (Multiply x y) name = Add (Multiply x (diff y name)) (Multiply y (diff x name))
diff (Variable var) name = if var == name then Constant 1 else Constant 0
a = Add (Constant 3) (Constant 4)
b = Add a (Variable "x")
c = Multiply b (Variable "y")
main = do
         putStrLn $ show $ evaluate a [("x", 3)]
         putStrLn $ show $ evaluate b [("x", 101)]
         putStrLn $ show $ evaluate c [("x", 101), ("y", 10)]
         putStrLn $ show c
         putStrLn $ show $ diff c "z"
         putStrLn $ show $ diff (Multiply (Variable "x") (Variable "x")) "x"
         putStrLn $ show b
