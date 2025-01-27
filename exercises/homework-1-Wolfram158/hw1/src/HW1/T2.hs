module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)

data N = Z | S N deriving (Show, Eq) -- Eq is used for tests

nplus :: N -> N -> N
nplus n Z            = n
nplus left (S right) = nplus (S left) right

nmult :: N -> N -> N
nmult _ Z = Z
nmult left right = nmult' left right left where
  nmult' acc (S Z) _            = acc
  nmult' acc Z _                = acc
  nmult' acc (S inside) summand = nmult' (nplus acc summand) inside summand

nsub :: N -> N -> Maybe N
nsub Z (S _)            = Nothing
nsub left Z             = Just left
nsub (S left) (S right) = nsub left right

ncmp :: N -> N -> Ordering
ncmp left right = case nsub left right of
  Nothing -> LT
  Just Z  -> EQ
  _       -> GT

nFromNatural :: Natural -> N
nFromNatural nat = convert nat Z where
  convert 0 n    = n
  convert nat' n = convert (nat' - 1) (S n)

nToNum :: Num a => N -> a
nToNum n = convert n 0 where
  convert Z acc          = acc
  convert (S inside) acc = convert inside (acc + 1)

nEven :: N -> Bool
nEven Z              = True
nEven (S Z)          = False
nEven (S (S inside)) = nEven inside

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv _ Z = error "division by zero not allowed"
ndiv up down = ndiv' up down Z where
  ndiv' up' down' acc = case nsub up' down' of
    Nothing     -> acc
    Just inside -> ndiv' inside down' (S acc)

nmod :: N -> N -> N
nmod up down = fromMaybe Z (nsub up $ nmult down $ ndiv up down)

-- nmod up down = fromJust $ nsub up $ nmult down $ ndiv up down
