module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' = fix $ \f g list -> if null list then [] else g (head list) : f g (tail list)

type Quadruple = (Natural, Natural, Natural, Natural)

mul :: Quadruple -> Quadruple -> Quadruple
mul (a11, a12, a21, a22) (b11, b12, b21, b22) = (x, y, z, t) where
  x = a11 * b11 + a12 * b21
  y = a11 * b12 + a12 * b22
  z = a21 * b11 + a22 * b21
  t = a21 * b12 + a22 * b22

fibHelper :: Quadruple -> Natural -> Quadruple
fibHelper = fix fun where
  fun f quadruple n
    | n == 0 = (1, 0, 0, 1)
    | even n = quad'
    | otherwise = quad' `mul` quadruple
    where
      quad = f quadruple (n `div` 2)
      quad' = quad `mul` quad

fib :: Natural -> Natural
fib = fix fun where
  fun _ 0 = 0
  fun _ 1 = 1
  fun _ n = let (x, _, _, _) = fibHelper (1, 1, 1, 0) (n - 1) in x

fac :: Natural -> Natural
fac = fix $ \f n -> if n == 0 then 1 else f (n - 1) * n

-- The second way to represent matrix while making function fib.
{-
data M22 a = M22 a a a a deriving Show

instance Num a => Num (M22 a) where
  M22 a11 a12 a21 a22 + M22 b11 b12 b21 b22 = M22 c11 c12 c21 c22 where
    c11 = a11 + b11
    c12 = a12 + b12
    c21 = a21 + b21
    c22 = a22 + b22
  M22 a11 a12 a21 a22 * M22 b11 b12 b21 b22 = M22 c11 c12 c21 c22 where
    c11 = a11 * b11 + a12 * b21
    c12 = a11 * b12 + a12 * b22
    c21 = a21 * b11 + a22 * b21
    c22 = a21 * b12 + a22 * b22
  abs (M22 a11 a12 a21 a22) = M22 (abs a11) (abs a12) (abs a21) (abs a22)
  signum (M22 a11 a12 a21 a22) = M22 (signum a11) (signum a12) (signum a21) (signum a22)
  fromInteger int = let fint = fromInteger int in M22 fint fint fint fint
  negate (M22 a11 a12 a21 a22) = M22 (negate a11) (negate a12) (negate a21) (negate a22)

fibHelper' :: M22 Natural -> Natural -> M22 Natural
fibHelper' = fix fun where
  fun f quadruple n
    | n == 0 = M22 1 0 0 1
    | even n = quad'
    | otherwise = quad' * quadruple
    where
      quad = f quadruple (n `div` 2)
      quad' = quad * quad

fib' :: Natural -> Natural
fib' = fix fun where
  fun _ 0 = 0
  fun _ 1 = 1
  fun _ n = let M22 x _ _ _ = fibHelper' (M22 1 1 1 0) (n - 1) in x
-}
