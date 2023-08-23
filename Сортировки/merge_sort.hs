merge :: [Integer] -> [Integer] -> (Integer -> Integer -> Bool) -> [Integer]
merge list1 [] signal = list1
merge [] list2 signal = list2
merge (x:xs) (y:ys) signal
    | x `signal` y = mconcat [[x], merge xs (y:ys) signal]
    | otherwise = mconcat [[y], merge (x:xs) ys signal]

main = do
          putStrLn "Done"
          putStrLn $ show $ merge [2, 3, 4] [-1, 4, 9, 11] (<)
          putStrLn $ show $ merge [4, 3, 2] [11, 9, 4, -1] (>)
