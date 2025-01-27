{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left x)       = (Left x, Left x)
distrib (Right (y, z)) = (Right y, Right z)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = let
  f (Right (Right c)) = Right c
  f (Right (Left b))  = Left $ Right b
  f (Left a)          = Left $ Left a
  g (Left (Left a))  = Left a
  g (Left (Right b)) = Right $ Left b
  g (Right c)        = Right $ Right c
  in
    Iso f g
