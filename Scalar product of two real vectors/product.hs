product (x:xs) (y:ys) = if xs == [] && ys == [] then x * y else if xs == [] || ys == [] then error "Vectors differ in length" else x * y + product xs ys

product vector1 vector2 = if not ((length vector1) == (length vector2)) then error "Vectors differ in length" else (sum (map (\(x, y) -> x*y) (zip vector1 vector2)))
