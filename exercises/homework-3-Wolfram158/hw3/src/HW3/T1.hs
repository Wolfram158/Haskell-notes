module HW3.T1
  ( Option (..)
  , Pair (..)
  , Quad (..)
  , Annotated (..)
  , Except (..)
  , Prioritised (..)
  , Stream (..)
  , List (..)
  , Fun (..)
  , Tree (..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  , ($)
  , (.)
  ) where

-- in T1 we can use only Show and Eq classes from Prelude
import Prelude (Eq, Show)

($) :: (a -> b) -> a -> b
f $ x = f x

infixr 0 $

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f $ g x

infixr 9 .

-- Laws:
-- 1. mapF id = id
-- 2. mapF f . mapF g = mapF (f . g)

data Option a = None | Some a
  deriving Show

-- (a -> b) -> Option a -> Option b
-- 1.1. mapF id None = None (true)
-- 1.2. mapF id (Some a) = Some (id a) = Some a (true)
-- 2.1. (mapF f . mapF g) None = mapF f (mapF g None) = mapF f None = None,
-- whereas mapF (f . g) None = None (true)
-- 2.2. (mapF f . mapF g) (Some a) = mapF f (mapF g (Some a)) =
-- mapF f (Some (g a)) = Some (f (g a)), whereas
-- mapF (f . g) (Some a) = Some ((f . g) a) (true)
mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None     = None
mapOption f (Some a) = Some $ f a

data Pair a = P a a
  deriving Show

-- 1. mapF id (P a b) = P (id a) (id b) = P a b
-- 2. (mapF f . mapF g) (P a b) = mapF f (mapF g (P a b)) =
-- mapF f (P (g a) (g b)) = P (f (g a)) (f (g b)), whereas
-- mapF (f . g) (P a b) = P ((f . g) a) ((f . g) b)
mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a b) = P (f a) (f b)

data Quad a = Q a a a a
  deriving (Show, Eq) -- Eq is used in tests

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a b c d) = Q (f a) (f b) (f c) (f d)

data Annotated e a = a :# e
  deriving (Show, Eq) -- Eq is used in tests

infix 0 :#

-- 1. Obviously.
-- 2. (mapF . mapF g) (a :# e) = mapF f (mapF g (a :# e)) =
-- mapF f (g a :# e) = f (g a) :# e, whereas
-- mapF (f . g) (a :# e) = (f . g) a :# e
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

data Except e a = Error e | Success a
  deriving Show

-- instance Functor (Either e) where...
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)     = Error e
mapExcept f (Success res) = Success $ f res

data Prioritised a = Low a | Medium a | High a
  deriving Show

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a)    = Low $ f a
mapPrioritised f (Medium a) = Medium $ f a
mapPrioritised f (High a)   = High $ f a

data Stream a = a :> Stream a
  deriving Show

infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (a :> rest) = f a :> mapStream f rest

data List a = Nil | a :. List a
  deriving (Show, Eq) -- Eq is used in tests

infixr 5 :.

-- instance Functor [] where...
mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil         = Nil
mapList f (a :. rest) = f a :. mapList f rest

-- hlint: Suggestion: Use newtype instead of data
data Fun i a = F (i -> a)

-- instance Functor ((->) i) where...
-- fmap :: (a -> b) -> f a -> f b
-- As f = (->) i, we have
-- (a -> b) -> ((->) i) a -> ((->) i) b which is the same as
-- (a -> b) -> (i -> a) -> (i -> b) which is the same as
-- (a -> b) -> (i -> a) -> i -> b which is composition (.)

-- As precaution, (.) is defined locally: comp f g x = f $ g x
-- Or mapFun f (F g) = F (\i -> f $ g i)
-- ($) also can be defined locally:
mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F $ f . g

data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving Show

-- 1. Obviously.
-- 2.1. (mapF f . mapF g) Leaf = mapF f $ mapF g Leaf =
-- mapF f Leaf = Leaf, whereas mapF (f . g) Leaf = Leaf
-- 2.2. (mapF f . mapF g) (Branch left val right) =
-- mapF f $ mapF g (Branch left val right) =
-- mapF f $ Branch (mapF g left) (g val) (mapF g right) =
-- Branch (mapF f $ mapF g left) (f $ g val) (mapF f $ mapF g right),
-- whereas mapF (f . g) (Branch left val right) =
-- Branch (mapF (f . g) left) ((f . g) val) (mapF (f . g) right)
mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree f (Branch left val right) = Branch left' val' right' where
  left' = mapTree f left
  val' = f val
  right' = mapTree f right
