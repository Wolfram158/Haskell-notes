merge :: [Integer] -> [Integer] -> Integer -> ([Integer], Integer)
merge list1 [] acc = (list1, acc)
merge [] list2 acc = (list2, acc)
merge (x:xs) (y:ys) acc = let case1 = merge xs (y:ys) 0
                              case2 = merge (x:xs) ys 0
                          in
                          if x < y then (mconcat [[x], fst case1], snd case1 + acc)
                          else (mconcat [[y], fst case2], snd case2 + acc + (fromIntegral (length (x:xs))))

mergeSort :: [Integer] -> Integer -> ([Integer], Integer)
mergeSort [x] acc = ([x], acc)
mergeSort [x, y] acc = merge [x] [y] acc
mergeSort list acc = let half = floor ((fromIntegral (length list)) / 2)
                         left = mergeSort (take half list) 0
                         right = mergeSort (drop half list) 0
                     in
                     merge (fst left) (fst right) (snd left + snd right)

countInversions :: [Integer] -> Integer
countInversions list = snd (mergeSort list 0)

main = do
          putStrLn $ show $ mergeSort [1, 2, 3, 11, 4, 5, 6, 7] 0
      {-  putStrLn $ show $ mergeSort [6, -1, 3, 14, 9, 3, 2, -3, -100, 10, 15, 23, 11] 0  -}
          putStrLn $ show $ mergeSort [1, 8, 5, 3, 7, 2, 6, 4, 9] 0
          putStrLn $ show $ merge [1, 3, 5, 7, 8] [2, 4, 6, 9] 0
          putStrLn $ show $ countInversions [8, 17, 1, 15, 14, 6, 4, 12, 11, 10, 20, 5, 9, 18, 13, 19, 2, 7, 16, 3]
          putStrLn $ show $ countInversions [6, 13, 11, 16, 8, 18, 9, 1, 10, 12, 20, 19, 7, 2, 15, 4, 17, 5, 14, 3]
          putStrLn $ show $ countInversions [2, 11, 15, 8, 6, 5, 14, 17, 18, 9, 4, 1, 10, 16, 13, 7, 3, 12]
          putStrLn $ show $ countInversions [26, 12, 3, 18, 27, 13, 7, 30, 25, 2, 20, 9, 15, 8, 28,
                                                        17, 11, 1, 16, 24, 23, 14, 5, 22, 6, 19, 10, 29, 21, 4]
