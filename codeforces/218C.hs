answer b s c nb ns nc pb ps pc r = f (read r) 0 (10 ^ 14) where
  max' forOne x n = max 0 (forOne * x - n)
  is_affordable amount mid = calculate_payment mid <= amount
  calculate_payment mid = pb * (max' b mid nb) + ps * (max' s mid ns) + pc * (max' c mid nc)   
  f amount l r = 
    let mid = (l + r) `div` 2 in 
      if l < r - 1 
      then
        if is_affordable amount mid then f amount mid r else f amount l mid
      else
        if is_affordable amount r then r else l
        
solve = do
  str <- getLine
  n <- getLine
  p <- getLine
  r <- getLine
  let nn = words n
  let nb = read $ head nn
  let ns = read $ head $ tail nn
  let nc = read $ last nn
  let pp = words p
  let pb = read $ head pp
  let ps = read $ head $ tail pp
  let pc = read $ last pp 
  let count chr str = length $ filter (== chr) str
  let b = count 'B' str
  let s = count 'S' str
  let c = count 'C' str
  let ans = answer b s c nb ns nc pb ps pc r 
  putStrLn (show ans)

main = solve
