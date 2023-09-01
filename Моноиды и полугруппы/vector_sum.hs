data Vector a = Vector [a] | Neutral | Incorrect deriving (Eq, Show)

instance Num a => Semigroup (Vector a) where
    Incorrect <> x = Incorrect
    x <> Incorrect = Incorrect
    x <> Neutral = x
    Neutral <> x = x
    (Vector a) <> (Vector b) = if length a == length b && length a > 0
                               then Vector $ zipWith (+) a b
                               else Incorrect

instance Num a => Monoid (Vector a) where
    mempty = Neutral

main = do
         putStrLn $ show $ Vector [1, 2, 3] <> Vector [3, 4, 5]
         putStrLn $ show $ Incorrect <> Vector [3, 4, 5]
         putStrLn $ show $ Incorrect <> Incorrect
         putStrLn $ show $ Vector [1, 2, 3] <> Vector [4, 5]
         putStrLn $ show $ Vector [1, 2, 3] <> Vector [4, 5, 6] <> Vector [7, 8, 9]
         putStrLn $ show $ Vector [1, 2, 3, 4, 5] <> mempty
         putStrLn $ show $ Vector [1] <> mempty
