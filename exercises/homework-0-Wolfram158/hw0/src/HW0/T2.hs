module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

doubleNeg :: a -> Not (Not a)
doubleNeg x y = y x

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg x y = x $ doubleNeg y
