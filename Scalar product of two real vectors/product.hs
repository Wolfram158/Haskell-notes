product (x:xs) (y:ys) = if xs == [] && ys == [] then x * y else if xs == [] || ys == [] then error "Vectors differ in length" else x * y + product xs ys
