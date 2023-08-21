import qualified Data.List as List

data Tree = Empty | Tree Tree (Integer, Integer) Tree deriving (Eq, Show)

split :: Tree -> Integer -> (Tree, Tree)
split Empty key = (Empty, Empty)
split (Tree left (x, y) right) key = let (left1, right1) = (split right x)
                                         (left2, right2) = (split left x)
                                     in
                                     if key > y then
                                         (Tree left (x, y) left1, right1)
                                     else
                                         (left2, Tree right2 (x, y) right)

merge :: Tree -> Tree -> Tree
merge (Tree left (x, y) right) Empty = Tree left (x, y) right
merge Empty (Tree left (x, y) right) = Tree left (x, y) right
merge (Tree left1 (x1, y1) right1) (Tree left2 (x2, y2) right2) = let tree1 = (merge right1 (Tree left2 (x2, y2) right2))
                                                                      tree2 = (merge (Tree left1 (x1, y1) right1) left2)
                                                                  in
                                                                  if y1 <= y2 then
                                                                      (Tree left1 (x1, y1) tree1)
                                                                  else
                                                                      (Tree tree2 (x2, y2) right2)

insert :: Tree -> (Integer, Integer) -> Tree
insert tree (x, y) = let (tree1, tree2) = (split tree x)
                         derived = (merge tree1 (Tree Empty (x, y) Empty))
                     in
                         merge derived tree2

{- remove :: Tree -> key -> Tree
remove Empty key = Empty
remove (Tree x y z) -}

sortByFirst (a1, b1) (a2, b2)
  | a1 < a2 = GT
  | a1 >= a2 = LT

buildImpl :: [Tree] -> Tree
buildImpl [] = Empty
buildImpl [Tree x y z] = Tree x y z
buildImpl ((Tree left (x, y) right):Empty:s) = buildImpl (s ++ [Tree left (x, y) right])
buildImpl (Empty:(Tree left (x, y) right):s) = buildImpl (s ++ [Tree left (x, y) right])
buildImpl (Empty:Empty:s) = buildImpl (s ++ [Empty])
buildImpl ((Tree left1 (x1, y1) right1):(Tree left2 (x2, y2) right2):s) = let a = (Tree left1 (x1, y1) right1)
                                                                              b = (Tree left2 (x2, y2) right2)
                                                                          in
                                                                          buildImpl (s ++ [merge b a])

build :: [(Integer, Integer)] -> Tree
build pairs = let sorted = List.sortBy sortByFirst pairs
                  mapped = map (\(x, y) -> (Tree Empty (x, y) Empty)) sorted
              in
              buildImpl (mapped ++ (replicate (2 ^ (ceiling (logBase 2 (fromIntegral (length mapped)))) - (length mapped)) Empty))

t = Tree Empty (3, -4) (Tree Empty (8, 10) Empty)
d = Tree (Tree (Tree Empty (20, 2) Empty) (30, 1) (Tree Empty (40, 3) Empty)) (50, 0)
                                   (Tree (Tree Empty (60, 5) Empty) (70, 4) (Tree Empty (80, 6) Empty))
pairs = [(30, 1), (50, 0), (60, 5), (70, 4), (8, 10), (3, -4), (20, 2), (40, 3), (80, 6)]

main = do
         putStrLn $ show $ split t 5
         putStrLn $ show $ split d 65
         putStrLn $ show $ merge t d
         putStrLn $ show $ insert Empty (10, 20)
         putStrLn $ show $ insert t (-8, -100)
         putStrLn $ show $ build [(20, 30), (14, 13), (-7, 1)]
         putStrLn $ show $ merge (Tree Empty (-7, 1) Empty) (merge (Tree Empty (14, 13) Empty) (Tree Empty (20, 30) Empty))
         putStrLn $ show $ build pairs
         putStrLn $ show $ build [(30, 1), (60, 5), (70, 4), (50, 0)]
