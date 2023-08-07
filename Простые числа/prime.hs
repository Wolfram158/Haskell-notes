hasDivisorInRange x (y:ys) = if x `mod` y == 0
                               then True
                               else if ys == []
                               then False
                               else hasDivisorInRange x ys
isPrime x = if x == 1
              then False
              else if x == 2
              then True
              else not (hasDivisorInRange (floor x) (take y [2..(1 + y)])) where y = (floor (sqrt (x :: Float)))
