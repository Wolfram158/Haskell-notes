module HW0.T6
  ( a
  , aWhnf
  , b
  , bWhnf
  , c
  , cWhnf
  ) where

import Data.Char (isSpace)
import HW0.T1 (distrib)

a :: (Either String a, Either String b)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))

aWhnf :: (Either String a, Either String b)
aWhnf = (Left x, Left x) where 
  x = "AB" ++ "CD" ++ "EF"

b :: [Bool]
b = map isSpace "Hello, World"

bWhnf :: [Bool]
bWhnf = isSpace 'H' : map isSpace "ello, World"

c :: String
c = if (1 :: Int) > 0 || error "X" then "Y" else "Z"

cWhnf :: String
cWhnf = ['Y']
