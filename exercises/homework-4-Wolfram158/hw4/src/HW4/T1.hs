{-# LANGUAGE LambdaCase #-}

module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad (ap, when)
import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f esa = ES $ \s ->
  case runES esa s of
    Error e          -> Error e
    Success (a :# e) -> Success $ f a :# e

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES $ \s -> Success $ x :# s

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState esesa = ES $ \s ->
  case runES esesa s of
    Error e             -> Error e
    Success (esa :# s1) -> runES esa s1

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success $ () :# f s

throwExceptState :: e -> ExceptState e s a
throwExceptState msg = ES $ const $ Error msg

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  estate >>= f = joinExceptState $ fmap f estate

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval = \case
  Op (Add x y) -> evalBinary x y (+) Add
  Op (Sub x y) -> evalBinary x y (-) Sub
  Op (Mul x y) -> evalBinary x y (*) Mul
  Op (Div x y) -> evalDiv x y
  Op (Abs x)   -> evalUnary x abs Abs
  Op (Sgn x)   -> evalUnary x signum Sgn
  Val x        -> return x

evalDiv :: Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
evalDiv x y = do
  left <- eval x
  right <- eval y
  when (right == 0) $ throwExceptState DivideByZero
  modifyExceptState $ (:) $ Div left right
  return $ left / right

evalBinary :: Expr -> Expr -> (Double -> Double -> Double) ->
  (Double -> Double -> Prim Double) ->
    ExceptState EvaluationError [Prim Double] Double
evalBinary x y fun ctor = do
  left <- eval x
  right <- eval y
  modifyExceptState $ (:) $ ctor left right
  return $ fun left right

evalUnary :: Expr -> (Double -> Double) ->
  (Double -> Prim Double) -> ExceptState EvaluationError [Prim Double] Double
evalUnary x fun ctor = do
  val <- eval x
  modifyExceptState $ (:) $ ctor val
  return $ fun val
