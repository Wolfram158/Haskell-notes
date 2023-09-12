-- (A -> C) & (B -> C) & (A \/ B) -> C

f :: (a -> c, b -> c, Either a b) -> c
f (g, h, Left x) = g x
f (g, h, Right x) = h x

main = do
          putStrLn $ show $ f (\x -> 100, \y -> 200, Left 3)
          putStrLn $ show $ f (\x -> 10, \y -> 20, Right "a")
          putStrLn $ show $ f (\x -> [1, 2, 3], \y -> [], Right 8)
