module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show, Eq) -- Eq is used for tests

infixr 5 :+

instance Semigroup (ListPlus a) where
  (Last lst) <> right      = lst :+ right
  (first :+ rest) <> right = first :+ (rest <> right)

data Inclusive a b = This a | That b | Both a b
  deriving (Show, Eq) -- Eq is used for tests

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This i <> This j     = This (i <> j)
  That i <> That j     = That (i <> j)
  Both x y <> Both z t = Both (x <> z) (y <> t)
  This i <> That j     = Both i j
  That i <> This j     = Both j i
  Both x y <> This i   = Both (x <> i) y
  Both x y <> That j   = Both x (y <> j)
  This i <> Both x y   = Both (i <> x) y
  That j <> Both x y   = Both x (j <> y)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  DS l@(_:_) <> DS r@(_:_) = DS $ l <> "." <> r
  DS str1 <> DS str2       = DS $ str1 <> str2

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F fun1 <> F fun2 = F (fun1 . fun2)

instance Monoid (Fun a) where
  mempty = F id
