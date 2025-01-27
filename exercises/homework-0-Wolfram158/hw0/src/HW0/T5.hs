module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz = \_ -> id

ns :: Nat a -> Nat a
ns n = \s z -> s (nToNum' s z n)

nplus :: Nat a -> Nat a -> Nat a
nplus n m = \s z -> nToNum' s (nToNum' s z m) n

nmult :: Nat a -> Nat a -> Nat a
nmult n m = \s z -> nToNum' (m s) z n

nFromNatural :: Natural -> Nat a
nFromNatural natural = helper natural nz where
  helper 0 acc        = acc
  helper natural' acc = helper (natural' - 1) (ns acc)

nToNum' :: (a -> a) -> a -> Nat a -> a
nToNum' s z nat = nat s z

{-
Base case. Let n = nz. Then (\_ -> id) (+1) 0 = 0.
Induction step. Let function nToNum works
fine for some n = nx, for which nToNum n = k.
Then nToNum (ns n) = (\s z -> s (nToNum n)) (+1) 0 =
(\s z -> s k) (+1) 0 = (\z -> (+1) k) 0 = (\z -> k + 1) 0 =
k + 1, as desired.
-}
nToNum :: Num a => Nat a -> a
nToNum = nToNum' (+1) 0
