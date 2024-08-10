import qualified Data.Map as Map
import Data.Maybe

modulo = 10 ^ 9 + 7
size = 2 * 10 ^ 5 + 10

p a b = (a * b) `mod` modulo

facts' = init 0 size [] where 
  init 0 size [] = 1:(init 1 size [1])
  init i size acc | i < size = let last' = p (head acc) i
                               in init (i + 1) size (last':acc)
                  | i == size = acc 

facts = Map.fromList $ zip [0..size] (reverse facts')

extended_euclid m n | n == 0 = [m, 1, 0]
                    | otherwise = let next = extended_euclid n (m `mod` n) 
                                  in 
                                    [head next, last next, 
                                    (head $ tail next) - (last next) * (m `div` n)]

__get j map = fromMaybe 0 $ Map.lookup j map

solve = do
  nk <- getLine
  let nk' = words nk
  let n = read $ head $ nk' :: Int
  let k = read $ last $ nk' :: Int
  a <- getLine
  let a' = words a
  let zero = length $ filter (== "0") a'
  let one = length $ filter (== "1") a'
  let numerator = p (__get one facts) (__get zero facts)
  let calculate j acc | j < one + 1 = if j < k + 1 && k - j < zero + 1
                                      then
                                        let left = p (__get j facts) (__get (one - j) facts)
                                            right = p (__get (k - j) facts) (__get (zero - k + j) facts)
                                            denum = p left right
                                            rev = head $ tail $ extended_euclid denum modulo
                                        in
                                          calculate (j + 1) ((acc + (p numerator rev)) `mod` modulo)
                                      else
                                        calculate (j + 1) acc
                      | otherwise = acc
  putStrLn $ show $ calculate ((k + 1) `div` 2) 0
 
solve_t t | t == 1 = solve
          | t > 1 = do
            solve
            solve_t (t - 1)
            
main = do
  t <- getLine
  solve_t (read t)
