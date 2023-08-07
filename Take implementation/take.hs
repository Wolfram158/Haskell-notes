myTake count (x:xs) = if count == 0 then [] else if xs == [] then [x] else [x] ++ (myTake (count - 1) xs)
