product1 :: Num a => [a] -> [a] -> Maybe a
product1 vec1 vec2
       | length vec1 /= length vec2 || length vec1 == 0 = Nothing
       | otherwise = Just $ sum $ zipWith (*) vec1 vec2
       
main = do
          putStrLn $ show $ product1 [1, 2, 3] [4, 5, 6]
          putStrLn $ show $ product1 [1, 2, 3] [4, 5, 6, 7]
          putStrLn $ show $ product1 [] []
