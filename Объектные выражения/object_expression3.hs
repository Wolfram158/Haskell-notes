import qualified Data.Map as Map

data Expression = Constant Float |
                  Variable String |
                  Add Expression Expression |
                  Multiply Expression Expression |
                  Divide Expression Expression |
                  Negate Expression |
                  Subtract Expression Expression deriving Eq

instance Show Expression where
  show (Constant value) = show value
  show (Variable name) = name
  show (Add x y) = mconcat ["(", show x, "+", show y, ")"]
  show (Multiply x y) = mconcat ["(", show x, "*", show y, ")"]
  show (Divide x y) = mconcat ["(", show x, "/", show y, ")"]
  show (Negate x) = mconcat ["-", "(", show x, ")"]
  show (Subtract x y) = mconcat ["(", show x, "-", show y, ")"]

evaluate :: Expression -> [(String, Float)] -> Float
evaluate (Add x y) args = (evaluate x args) + (evaluate y args)
evaluate (Multiply x y) args = (evaluate x args) * (evaluate y args)
evaluate (Constant value) args = value
evaluate (Variable name) args = ((Map.fromList args) Map.! name)
evaluate (Divide x y) args = (evaluate x args) / (evaluate y args)
evaluate (Negate x) args = -(evaluate x args)
evaluate (Subtract x y) args = (evaluate x args) - (evaluate y args)

diff :: Expression -> String -> Expression
diff (Add x y) name = Add (diff x name) (diff y name)
diff (Constant value) name = Constant 0
diff (Multiply x y) name = Add (Multiply x (diff y name)) (Multiply y (diff x name))
diff (Variable var) name = if var == name then Constant 1 else Constant 0
diff (Divide x y) name =
         Divide (Subtract (Multiply y (diff x name)) (Multiply x (diff y name))) (Multiply y y)
diff (Subtract x y) name = Subtract (diff x name) (diff y name)

simplify :: Expression -> Expression
simplify (Add (Constant 0) y) = y
simplify (Add x (Constant 0)) = x
simplify (Multiply (Constant 0) y) = Constant 0
simplify (Multiply x (Constant 0)) = Constant 0
simplify (Multiply (Constant 1) y) = simplify y
simplify (Multiply x (Constant 1)) = simplify x
simplify (Divide (Constant 0) (Constant y)) = if not (y == 0) then Constant 0 else error "Division by zero"
simplify (Divide (Constant x) (Constant y)) = if not (y == 0) then Constant (x / y)
                                              else error "Division by zero"
simplify (Multiply (Constant x) (Constant y)) = Constant (x * y)
simplify (Add (Constant x) (Constant y)) = Constant (x + y)
simplify (Variable name) = Variable name
simplify (Constant value) = Constant value
simplify (Negate (Constant value)) = Constant (-value)
simplify (Add x y) = if x == simplify x && y == simplify y then Add x y
                     else simplify (Add (simplify x) (simplify y))
simplify (Divide x y) = if x == simplify x && y == simplify y then Divide x y
                        else simplify (Divide (simplify x) (simplify y))
simplify (Multiply x y) = if x == simplify x && y == simplify y then Multiply x y
                          else simplify (Multiply (simplify x) (simplify y))
simplify (Negate (Constant 0)) = Constant 0
simplify (Negate x) = (Negate (simplify x))

toMiniString :: Expression -> String
toMiniString (Negate (Constant value)) = show (-value)

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
         putStrLn $ show $ simplify $ Divide (Constant 0) (Constant 7)
         putStrLn $ show $ simplify $ diff (Divide (Multiply (Variable "x") (Variable "y"))
                                                                 (Add (Constant 3) (Constant 4))) "x"
