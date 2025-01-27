{-# LANGUAGE LambdaCase #-}

module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1
import Prelude (Monoid, Semigroup, mempty, (<>))

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a b, P c d) = P (a, c) (b, d)

wrapPair :: a -> Pair a
wrapPair val = P val val

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a b c d, Q e f g h) = Q (a, e) (b, f) (c, g) (d, h)

wrapQuad :: a -> Quad a
wrapQuad val = Q val val val val

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a1 :# e1, a2 :# e2) = (a1, a2) :# e1 <> e2

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated val = val :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e

-- instance Functor (Except e) where
--   fmap = mapExcept

-- instance Applicative (Except e) where
--   pure = Success
--   Error e <*> _ = Error e
--   Success f <*> r = fmap f r

-- sequenceA (Except e a, Except e b) gives Except e (Except e a, b),
-- not Except e (a, b)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised = \case
  (Low a, Medium b)    -> Medium (a, b)
  (Low a, High b)      -> High (a, b)
  (Medium a, High b)   -> High (a, b)
  (Low a, Low b)       -> Low (a, b)
  (Medium a, Medium b) -> Medium (a, b)
  (High a, High b)     -> High (a, b)
  (High a, Low b)      -> High (a, b)
  (High a, Medium b)   -> High (a, b)
  (Medium a, Low b)    -> Medium (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a1 :> rest1, a2 :> rest2) = (a1, a2) :> distStream (rest1, rest2)

wrapStream :: a -> Stream a
wrapStream val = val :> wrapStream val

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (anil@(a :. Nil), b :. rest) = (a, b) :. distList (anil, rest)
distList (a :. rest, right) =
  let list1 = distList (a :. Nil, right)
      list2 = distList (rest, right)
      concatenate Nil rest'               = rest'
      concatenate (start :. rest') right' = start :. concatenate rest' right'
  in concatenate list1 list2

wrapList :: a -> List a
wrapList val = val :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f1, F f2) = F $ \i -> (f1 i, f2 i)

const :: a -> b -> a
const x _ = x

wrapFun :: a -> Fun i a
wrapFun val = F $ const val
