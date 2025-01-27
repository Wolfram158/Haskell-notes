module HW2.T3
  ( epart
  , mcat
  ) where

{-
--hlint recommends using fromMaybe
(which needs line "import Data.Maybe (fromMaybe)") there, however,
using only Foldable methods is expected in
implementation of this function.
-}
mcat :: Monoid a => [Maybe a] -> a
mcat maybes = case foldr (<>) mempty maybes of
  Just value -> value
  Nothing    -> mempty

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap fun where
  fun (Left msg)  = (msg, mempty)
  fun (Right val) = (mempty, val)
