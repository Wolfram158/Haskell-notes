newtype Adam a = Adam { descent :: Int -> ([a] -> a) -> ([a] -> [a]) -> [a] -> a -> a -> a
                                   -> a -> a -> Int -> IO () }

adam_impl = Adam {
descent = \n -> (\f -> (\df -> (\start -> (\b1 -> (\b2 -> (\eps
-> (\lr -> (\cond -> (\maxiter -> do
  let current_v = replicate n 0
  let current_G = current_v
  let inf = 1 / 0
  let last_x = replicate n inf
  let norm a b = sum $ map (^2) $ zipWith (-) a b
  let sqr = map (^2)
  let mul num = map (*num)
  let plus = zipWith (+)
  let minus = zipWith (-)
  let delta = zipWith (\x y -> lr * y / sqrt (x + eps))
  let answer v g last x iter | iter == maxiter = (x, f x, iter)
                             | norm last x < cond = (x, f x, iter)
                             | otherwise = let diff = df x
                                               next_v = plus (mul b1 v) (mul (1 - b1) diff)
                                               next_G = plus (mul b2 g) (mul (1 - b2) (sqr diff))
                                               next_x = minus x (delta next_G next_v)
                                           in
                                               answer next_v next_G x next_x (iter + 1)
  let desired = answer current_v current_G last_x start 0
  let (sel1, sel2, sel3) = desired
  let print name value = putStrLn $ name <> (show value)
  print "Found min point: " sel1
  print "Value of function at found min point: " sel2
  print "Count of iterations: " sel3
  )))))))))
}

adam n f df start lr cond maxiter = descent adam_impl n f df start 0.9 0.99 1e-8 lr cond maxiter

quad [x] = x ^ 2 - 17 * x + 3
quad_dx [x] = [2 * x - 17]

rosenbrock (m:n:mns) = let addition x y = (1 - x) ^ 2 + 100 * (y - x ^ 2) ^ 2
                           f v1 v2 (x:xs) acc = f v2 x xs (acc + addition v1 v2)
                           f v1 v2 [] acc = acc + addition v1 v2
                       in f m n mns 0

rosenbrock_dx (m:ms) = let addition z x y = 200 * (x - z ^ 2) - 400 * x * (y - x ^ 2) - 2 * (1 - x)
                           f True r v (x:xs) acc = f False v x xs (addition r v x - 200 * (v - v ^ 2):acc)
                           f False r v (x:xs) acc = f False v x xs (addition r v x:acc)
                           f False r v [] acc = 200 * (v - r ^ 2):acc 
                       in reverse $ f True m m ms []

squares l = sum $ map (^2) l
squares_dx l = map (*2) l
 
delimiter = replicate 60 '_'

test_template n f df start lr cond maxiter = do
  adam n f df start lr cond maxiter
  putStrLn delimiter

rosenbrock_test n start lr cond maxiter = test_template n rosenbrock rosenbrock_dx start lr cond maxiter

squares_test n start lr cond maxiter = test_template n squares squares_dx start lr cond maxiter

main = do
  adam 1 quad quad_dx [1] 0.1 1e-5 1000
  putStrLn delimiter
  rosenbrock_test 2 [3, 4] 0.1 1e-5 1000
  rosenbrock_test 2 [3, 4] 0.1 1e-15 100000
  rosenbrock_test 2 [3, 4] 0.01 1e-15 100000
  rosenbrock_test 2 [-3, 5] 0.005 1e-15 10000
  rosenbrock_test 4 [-3, 5, -2, -1] 0.03 1e-30 100000
  rosenbrock_test 4 [-3, 5, -2, -1] 0.3 1e-30 100000
  rosenbrock_test 4 [1.4, 0.5, 2, -1] 0.00003 1e-30 100000
  squares_test 4 [10..13] 0.0003 1e-30 100000
  squares_test 8 [10..17] 0.0003 1e-30 100000
  squares_test 32 [10..41] 0.003 1e-10 100000
