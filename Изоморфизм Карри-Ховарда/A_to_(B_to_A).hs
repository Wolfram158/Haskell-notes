-- A -> B -> A

f :: a -> (b -> a)
f x = \y -> x

main = do
         putStrLn $ show $ f 3 "xyz"
         putStrLn $ show $ f "abc" "def"
         putStrLn $ show $ f 5 [1, 2, 3]
