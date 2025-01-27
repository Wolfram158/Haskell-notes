module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  , mkBranch -- to use in tests
  ) where

{-
Depth and size were wrapped into newtypes to prevent
mixing up depth and size in places.
-}
newtype Depth = D Int deriving (Show, Eq)
newtype Size = S Int deriving (Show, Eq)
type Meta = (Depth, Size)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show, Eq) -- Eq is used for tests

tsize :: Tree a -> Int
tsize Leaf                       = 0
tsize (Branch (_, S size) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                        = 0
tdepth (Branch (D depth, _) _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember target (Branch _ left current right)
  | target == current = True
  | target < current = tmember target left
  | otherwise = tmember target right

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch tree1 val tree2 =
  let depth = max (tdepth tree1) (tdepth tree2) + 1
      size = tsize tree1 + tsize tree2 + 1
  in
    Branch (D depth, S size) tree1 val tree2

{- decomposeBranch :: Tree a -> (Tree a, a, Tree a)
decomposeBranch tree = case tree of
  Branch _ left value right -> (left, value, right)
  Leaf                      -> error "can not decompose leaf" -}

decomposeBranch :: a -> Tree a -> (Tree a, a, Tree a)
decomposeBranch defaultValue tree = case tree of
  Branch _ left value right -> (left, value, right)
  Leaf                      -> (Leaf, defaultValue, Leaf)

rotateLeft, rotateRight :: Tree a -> a -> Tree a -> Tree a
rotateLeft left value right =
  let (left1, b, right1) = decomposeBranch value right
      tdleft1 = tdepth left1
      tdright1 = tdepth right1
      resLeft1 = mkBranch left value left1
      (left2, c, right2) = decomposeBranch value left1
      resRight2 = mkBranch right2 b right1
      resLeft2 = mkBranch left value left2
  in
    if tdleft1 == tdright1 || tdleft1 == tdright1 - 1
      then
        mkBranch resLeft1 b right1
      else
        mkBranch resLeft2 c resRight2

rotateRight left value right =
  let (left1, b, right1) = decomposeBranch value left
      tdleft1 = tdepth left1
      tdright1 = tdepth right1
      resRight1 = mkBranch right1 value right
      (left2, c, right2) = decomposeBranch value right1
      resLeft2 = mkBranch left1 b left2
      resRight2 = mkBranch right2 value right
  in
    if tdleft1 == tdright1 || tdleft1 == tdright1 + 1
      then
        mkBranch left1 b resRight1
      else
        mkBranch resLeft2 c resRight2

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance tree@(Branch _ left value right) = case tdepth left - tdepth right of
  -2 -> rotateLeft left value right
  2  -> rotateRight left value right
  _  -> tree

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert value Leaf  = mkBranch Leaf value Leaf
tinsert value tree@(Branch _ left val right)
  | value == val = tree
  | value < val =
    let newBranch = tinsert value left
    in
      balance $ mkBranch newBranch val right
  | otherwise =
    let newBranch = tinsert value right
    in
      balance $ mkBranch left val newBranch

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

{-
tFromList values = tFromList' values Leaf where
  tFromList' [] tree     = tree
  tFromList' (v:vs) tree = tFromList' vs (tinsert v tree)
 -}
