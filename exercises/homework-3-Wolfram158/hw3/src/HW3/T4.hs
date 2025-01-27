{-# LANGUAGE LambdaCase #-}

module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import Control.Monad (ap)
import HW3.T1 hiding (($), (.))

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S { runS = z } where
  z s = y where
    x :# s1 = runS state s
    y = f x :# s1

wrapState :: a -> State s a
wrapState val = S { runS = \s -> val :# s }

joinState :: State s (State s a) -> State s a
joinState state = S { runS = z } where
  z s = y where
    x :# s1 = runS state s
    y = runS x s1

modifyState :: (s -> s) -> State s ()
modifyState f = S { runS = \s -> () :# f s }

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) = ap

instance Monad (State s) where
  state >>= f = joinState $ fmap f state

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show, Eq) -- Eq is used for test

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y = Op $ Add x y
  x - y = Op $ Sub x y
  x * y = Op $ Mul x y
  abs x = Op $ Abs x
  signum x = Op $ Sgn x
  fromInteger x = Val $ fromInteger x

instance Fractional Expr where
  x / y = Op $ Div x y
  fromRational x = Val $ fromRational x

evalBinary :: Expr -> Expr -> (Double -> Double -> Double) ->
  (Double -> Double -> Prim Double) -> State [Prim Double] Double
evalBinary x y fun ctor = do
  left <- eval x
  right <- eval y
  modifyState (ctor left right :)
  return $ fun left right

-- The following way also works
-- (although hlint recommends removing braces surrounding
-- return $ fun x' y' which leads to impossibility to compile):
-- evalBinary x y fun ctor =
--   eval x >>= \x' -> eval y >>= \y' ->
--     modifyState (ctor x' y' :) >> (return $ fun x' y')

evalUnary :: Expr -> (Double -> Double) ->
  (Double -> Prim Double) -> State [Prim Double] Double
evalUnary x fun ctor = do
  val <- eval x
  modifyState (ctor val :)
  return $ fun val

eval :: Expr -> State [Prim Double] Double
eval = \case
  Op (Add x y) -> evalBinary x y (+) Add
  Op (Sub x y) -> evalBinary x y (-) Sub
  Op (Mul x y) -> evalBinary x y (*) Mul
  Op (Div x y) -> evalBinary x y (/) Div
  Op (Abs x)   -> evalUnary x abs Abs
  Op (Sgn x)   -> evalUnary x signum Sgn
  Val x        -> return x
