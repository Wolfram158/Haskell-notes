dec :: (Ord a, Num a) => (a, b) -> Either a b
dec (a, b)
   | a < 100 = Left a
   | otherwise = Right b

main = do
         putStrLn $ show $ dec (8, 34.5)
         putStrLn $ show $ dec (100, 9)
