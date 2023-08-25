isLeft :: Int -> Int -> Bool -> Int
isLeft l r flag
     | flag == True = l
     | otherwise = r

binarySearchImpl :: Ord a => [a] -> Int -> Int -> a -> (a -> a -> Bool) -> (a -> a -> Bool) -> Bool -> Int
binarySearchImpl arr l r x sign1 sign2 flag = let mid = (floor ((fromIntegral (l + r)) / 2))
                                                  _isLeft = isLeft l r flag
                                                  _notIsLeft = isLeft l r (not flag)
                                              in
                                              if l < r - 1
                                              then
                                                if (arr !! mid) `sign1` x
                                                then
                                                binarySearchImpl arr mid r x sign1 sign2 flag
                                                else
                                                binarySearchImpl arr l mid x sign1 sign2 flag
                                              else
                                                if (arr !! _isLeft) `sign2` x
                                                then
                                                _isLeft
                                                else if (arr !! _notIsLeft) `sign2` x
                                                then
                                                _notIsLeft
                                                else -1

binarySearch list x sign1 sign2 flag = binarySearchImpl list 0 (length list - 1) x sign1 sign2 flag

main = do
          putStrLn $ show $ binarySearchImpl [1, 8, 100, 200, 300, 500, 1000] 0 6 100 (<) (<) False
          putStrLn $ show $ binarySearch [1, 8, 100, 200, 200, 200, 300, 500, 1000] 200 (<) (==) False
          putStrLn $ show $ binarySearch [1, 8, 100, 200, 200, 200, 300, 500, 1000] 200 (<) (==) True
          putStrLn $ show $ binarySearch [1, 8, 100, 200, 200, 200, 300, 500, 1000] 201 (<=) (<=) False
          putStrLn $ show $ binarySearch [1, 8, 100, 200, 200, 200, 300, 500, 1000] 200 (<) (<) False
          putStrLn $ show $ binarySearch [70, 30, 30, 30, 20, 10, 10, 10, 5, 4, 3, 2] 6 (>=) (<) True
