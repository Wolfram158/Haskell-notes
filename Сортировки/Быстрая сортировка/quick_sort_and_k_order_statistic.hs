quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort list = let mid = list !! floor (fromIntegral (length list) / 2)
                           in
                           (quickSort [t | t <- list, t < mid]) ++
                           [t | t <- list, t == mid] ++
                           (quickSort [t | t <- list, t > mid])

statistic :: Ord a => [a] -> Int -> Maybe a
statistic [x] 0 = Just x
statistic [x] _ = Nothing
statistic list k
       | k >= length list = Nothing
       | length less <= k && greater == [] = Just (equal !! (k - length less))
       | length less <= k = statistic (equal ++ greater) (k - length less)
       | otherwise = statistic less k
       where mid = list !! floor (fromIntegral (length list) / 2)
             less = [t | t <- list, t < mid]
             equal = [t | t <- list, t == mid]
             greater = [t | t <- list, t > mid]

main = do
         putStrLn $ show $ quickSort [8, 3, 5, 1, 0, -3, 4]
         putStrLn $ show $ quickSort [9, 3, 2, -10, 41, 2, 2, 3, -4, -5, -6, 5, 3, 1, 8]
         putStrLn $ show $ statistic [9, 3, 2, -10, 41, 2, 2, 3, -4, -5, -6, 5, 3, 1, 8] 5
         putStrLn $ show $ statistic [8, 3, 5, 1, 0, -3, 4] 2
         putStrLn $ show $ statistic [-2, 4, 9, 0, -5, -8, -7, 9, -9, -6, -9, 1, 3, -3, 9, -3, -8, 6, -5, 10] 0
         putStrLn $ show $ statistic [-2, 4, 9, 0, -5, -8, -7, 9, -9, -6, -9, 1, 3, -3, 9, -3, -8, 6, -5, 10] 8
         putStrLn $ show $ statistic [-2, 4, 9, 0, -5, -8, -7, 9, -9, -6, -9, 1, 3, -3, 9, -3, -8, 6, -5, 10] 100
