merge :: [Integer] -> [Integer] -> (Integer -> Integer -> Bool) -> [Integer]
merge list1 [] signal = list1
merge [] list2 signal = list2
merge (x:xs) (y:ys) signal
    | x `signal` y = mconcat [[x], merge xs (y:ys) signal]
    | otherwise = mconcat [[y], merge (x:xs) ys signal]

mergeSort :: [Integer] -> (Integer -> Integer -> Bool) -> [Integer]
mergeSort [x] signal = [x]
mergeSort [x, y] signal = merge [x] [y] signal
mergeSort list signal = let len = floor ((fromIntegral (length list)) / 2)
                        in
                        merge (mergeSort (take len list) signal) (mergeSort (drop len list) signal) signal

main = do
          putStrLn $ show $ merge [2, 3, 4, 8, 10, 100] [-1, 4, 9, 11] (<)
          putStrLn $ show $ merge [4, 3, 2, 0, -1] [11, 9, 4, -1] (>)
          putStrLn $ show $ mergeSort [3, -5, 9, 11, -5, -2, 1, 2, 4] (<)
          putStrLn $ show $ mergeSort [7, -1, -2, 3, 4, -5, 6, 6, 100, 93, 13, 2] (>)
