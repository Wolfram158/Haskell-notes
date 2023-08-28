transposeImpl :: [[a]] -> [[a]] -> [a] -> Int -> Int -> Int -> Int -> [[a]]
transposeImpl arr result resultX lenX lenY i j
              | i < lenY = transposeImpl arr result (resultX ++ [((arr !! i) !! j)]) lenX lenY (i + 1) j
              | j < lenX = transposeImpl arr (result ++ [resultX]) [] lenX lenY 0 (j + 1)
              | otherwise = result

transpose :: [[a]] -> [[a]]
transpose arr = transposeImpl arr [] [] (length (arr !! 0)) (length arr) 0 0

main = do
         putStrLn $ show $ transpose [[1, 2], [3, 4]]
         putStrLn $ show $ transpose [[1, 2], [3, 4], [5, 6]]
         putStrLn $ show $ transpose [[1, 2, 3], [4, 5, 6]]
         putStrLn $ show $ transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
         putStrLn $ show $ transpose [[1]]
