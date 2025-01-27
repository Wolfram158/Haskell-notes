module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr fun ([] :| []) where
  fun current (x :| xs)
    | current == sep = [] :| (x : xs)
    | otherwise = (current : x) :| xs

{-
Example. Let we need to join [a, b, c, d, e] with symbol s.
Then (a <> (s:(b <> (s:(c <> (s:(d <> (s:e)))))))) will take
O((1 + length(d)) + (1 + length(c)) + (1 + length(b)) +
(1 + length(a))) = O(length(d) + length(c) + length(b) + length(a)) =
O(length(e) + length(d) + length(c) + length(b) + length(a)) actions,
as desired.
-}
joinWith :: a -> NonEmpty [a] -> [a]
joinWith bridge = foldr1 (\s1 s2 -> s1 <> (bridge : s2))
