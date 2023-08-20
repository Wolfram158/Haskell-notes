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
diff (Negate x) name = Negate (diff x name)

getId :: Expression -> Integer
getId (Constant value) = 1
getId (Variable name) = 2
getId (Add x y) = 3
getId (Multiply x y) = 4
getId (Subtract x y) = 5
getId (Divide x y) = 6
getId (Negate x) = 7

getIdsOfMulChilds :: Expression -> [Integer]
getIdsOfMulChilds (Multiply x y) = [getId x, getId y]

simplify :: Expression -> Expression
simplify (Add (Constant 0) y) = simplify y
simplify (Add x (Constant 0)) = simplify x
simplify (Subtract x (Constant 0)) = simplify x
simplify (Subtract (Constant 0) y) = Negate y
simplify (Multiply (Constant 0) y) = Constant 0
simplify (Multiply x (Constant 0)) = Constant 0
simplify (Multiply (Constant 1) y) = simplify y
simplify (Multiply x (Constant 1)) = simplify x
simplify (Divide (Constant 0) (Constant y)) = if not (y == 0) then Constant 0 else error "Division by zero"
simplify (Divide (Constant x) (Constant y)) = if not (y == 0) then Constant (x / y)
                                              else error "Division by zero"
simplify (Divide (Multiply (Constant x) y) (Constant z)) = if not (z == 0) then Divide y (Constant (z / x))
                                                           else error "Division by zero"
simplify (Divide (Multiply x (Constant y)) (Constant z)) = if not (z == 0) then Divide x (Constant (z / y))
                                                           else error "Division by zero"
simplify (Multiply (Constant x) (Constant y)) = Constant (x * y)
simplify (Add (Constant x) (Constant y)) = Constant (x + y)
simplify (Subtract (Constant x) (Constant y)) = Constant (x - y)
simplify (Variable name) = Variable name
simplify (Constant value) = Constant value
simplify (Negate (Constant 0)) = Constant 0
simplify (Negate (Constant value)) = Constant (-value)
simplify (Negate (Negate x)) = simplify x
simplify (Add x y) = if simplify x == Constant 0 || simplify y == Constant 0 || (getId (simplify x) == 1 && getId (simplify y) == 1)
                     then simplify (Add (simplify x) (simplify y))
                     else (Add (simplify x) (simplify y))
simplify (Subtract x y) = if simplify x == Constant 0 || simplify y == Constant 0 || (getId (simplify x) == 1 && getId (simplify y) == 1)
                          then simplify (Subtract (simplify x) (simplify y))
                          else (Subtract (simplify x) (simplify y))
simplify (Divide x y) = if simplify x == Constant 0 ||
                                        (getId (simplify x) == 1 && getId (simplify y) == 1) ||
                                        (getId (simplify x) == 4 && ((getIdsOfMulChilds (simplify x)) !! 0 == 1)) ||
                                                    (getId (simplify x) == 4 && ((getIdsOfMulChilds (simplify x)) !! 1 == 1))
                        then simplify (Divide (simplify x) (simplify y))
                        else (Divide (simplify x) (simplify y))
simplify (Multiply x y) = if simplify x == Constant 0 ||
                                      simplify x == Constant 1 ||
                                               simplify y == Constant 0 ||
                                                            simplify y == Constant 1 ||
                                                                (getId (simplify x) == 1 && getId (simplify y) == 1)
                          then simplify (Multiply (simplify x) (simplify y))
                          else (Multiply (simplify x) (simplify y))
simplify (Negate x) = Negate (simplify x)

getSign :: Expression -> String
getSign (Add x y) = "+"
getSign (Subtract x y) = "-"
getSign (Multiply x y) = "*"
getSign (Divide x y) = "/"

takeBrackets :: Bool -> Expression -> String
takeBrackets brackets operand = if brackets == True then mconcat ["(", toMiniString operand, ")"]
                                                    else toMiniString operand
getPriority :: Expression -> Integer
getPriority (Add x y) = 0
getPriority (Subtract x y) = 0
getPriority (Multiply x y) = 1
getPriority (Divide x y) = 1
getPriority (Constant value) = 2
getPriority (Variable name) = 2
getPriority (Negate x) = 3

needBrackets :: Expression -> Bool
needBrackets (Divide x y) = True
needBrackets (Subtract x y) = True
needBrackets expr = False

toMiniStringImpl :: (Expression -> Expression -> Expression) -> Expression -> Expression -> String
toMiniStringImpl ctor x y = mconcat [takeBrackets ((getPriority x) < (getPriority (ctor x y))) x,
                                     getSign (ctor x y),
                                     takeBrackets ((getPriority y) < (getPriority (ctor x y)) ||
                                                   (getPriority (ctor x y)) == (getPriority y) &&
                                                   ((needBrackets (ctor x y)) || (needBrackets y))) y]

toMiniString :: Expression -> String
toMiniString (Constant value) = show value
toMiniString (Variable name) = name
toMiniString (Negate (Constant value)) = show (-value)
toMiniString (Negate (Variable name)) = mconcat ["-", name]
toMiniString (Negate (Subtract x y)) = mconcat ["-", "(", toMiniString (Subtract x y), ")"]
toMiniString (Negate (Add x y)) = mconcat ["-", "(", toMiniString (Add x y), ")"]
toMiniString (Negate (Multiply x y)) = mconcat ["-", toMiniString (Multiply x y)]
toMiniString (Negate (Divide x y)) = mconcat ["-", toMiniString (Divide x y)]
toMiniString (Negate (Negate x)) = mconcat ["-", "(", toMiniString (Negate x), ")"]
toMiniString (Add x y) = toMiniStringImpl Add x y
toMiniString (Subtract x y) = toMiniStringImpl Subtract x y
toMiniString (Multiply x y) = toMiniStringImpl Multiply x y
toMiniString (Divide x y) = toMiniStringImpl Divide x y

a = Add (Constant 3) (Constant 4)
b = Add a (Variable "x")
c = Multiply b (Variable "y")
d = Multiply (Add (Constant 3) (Divide (Multiply (Constant 2) (Variable "x")) (Variable "y"))) (Constant 6)

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
         putStrLn $ show $ simplify $ diff c "z"
         putStrLn $ show $ simplify $ Add (Add (Constant 0) (Constant 0)) (Constant 0)
         putStrLn $ toMiniString c
         putStrLn $ toMiniString d
         putStrLn $ toMiniString $ simplify $ Add (Add (Multiply (Constant 0) (Variable "x")) (Constant 3)) (Constant 4)
