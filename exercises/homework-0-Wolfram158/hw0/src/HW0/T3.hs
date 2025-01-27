module HW0.T3
  ( compose
  , contract
  , i
  , k
  , permute
  , s
  ) where

-- S
s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

-- K
k :: a -> b -> a
k x _ = x

{-
Define T-combinator as T x y = y x.
Then T x y = y x = (K y (K y)) x =
= (S K K y) x = ((S K K) y) x,
so y = (S K K) y and then I = S K K
-}
-- I
i :: a -> a
i = s k k

{-
B :: (b -> c) -> (a -> b) -> a -> c
B x y z = x (y z) = K x z (y z) = (K x) z (y z) =
= S (K x) y z = K S x (K x) y z = (K S) x (K x) y z =
= ((K S) x (K x)) y z = (S (K S) K x) y z = (S (K S) K) x y z,
so B = S (K S) K
-}
-- B
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

{-
W :: (a -> a -> b) -> a -> b
W x y = x y y = x y (K y (x y)) = x y (S K xy) =
= x y ((S K x) y) = S x (S K x) y = S x ((S K) x) y =
= (S x ((S K) x)) y = (S S (S K) x) y = (S S (S K)) x y,
so W = S S (S K)
-}
-- W
contract :: (a -> a -> b) -> (a -> b)
contract = s s (s k)

{-
We can use following algorithm from one book
devoted to combinatory logic:
(1) \x.x = I,
(2) \x.P = K P, if x does not belong to FV(P),
(3) \x.P Q = S (\x.P) (\x.Q) otherwise.
As function permute is the same as lambda expression \x.\y.\z.x z y,
we can apply this algorithm to lambda expression above and get that
permute = s (s (k s) (s (k k) (s (k s) (s (s (k s) (s (k k) i)) (k i))))) (k (s (k k) i)).
-}
-- C
permute :: (a -> b -> c) -> (b -> a -> c)
permute = s part1 part2 where
  part1 = s (k s) (s (k k) (s (k s) (s (s (k s) (s (k k) (s k k))) (k (s k k)))))
  part2 = k (s (k k) (s k k))
