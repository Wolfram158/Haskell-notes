module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch Int (Tree a) a (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldr _ acc Leaf = acc
  foldr f acc (Branch _ left val right) = foldr f newAcc left where
    newAcc = f val $ foldr f acc right

-- The way to get implementation of foldr (tfoldr) for free
-- instance Foldable Tree where
--   foldMap _ Leaf = mempty
--   foldMap fun (Branch _ left val right) = result where
--     result = foldMap fun left <> fun val <> foldMap fun right

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr = foldr
