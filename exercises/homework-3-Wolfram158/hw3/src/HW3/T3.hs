{-# OPTIONS_GHC -fno-warn-orphans #-}

module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import Control.Monad (ap, join)
import HW3.T1 hiding (($), (.))
import HW3.T2

joinOption :: Option (Option a) -> Option a
joinOption None       = None
joinOption (Some val) = val

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)     = Error e
joinExcept (Success res) = res

-- The following condition must be satisfied:
-- distF (p, q) = joinF (mapF (\a -> mapF (\b -> (a, b)) q) p)

-- Let q = (x1 :# y1) :# t1, p = (x2 :# y2) :# t2
-- According to definition of mapF (mapAnnotated) in T1,
-- (\a -> mapF (\b -> (a, b)) q) = \a -> (a, x1 :# y1) :# t1,
-- mapF (\a -> (a, x1 :# y1) :# t1) p = ((x2 :# y2, x1 :# y1) :# t1) :# t2, so
-- the following condition must be satistied:
-- distF (p, q) = joinF (((x2 :# y2, x1 :# y1) :# t1) :# t2)
-- According to definition of distF (distAnnotated) in T2,
-- distF (p, q) = (x2 :# y2, x1 :# y1) :# t2 <> t1, so
-- joinF ((a1 :# e1) :# e2) = a1 :# (e2 <> e1), not a1 :# (e1 <> e2),
-- as supposed at the first time.
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a1 :# e1) :# e2) = a1 :# (e2 <> e1)

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (list :. Nil) = list
joinList (list :. rest) =
  let concatenate Nil rest'            = rest'
      concatenate (start :. end) rest' = start :. concatenate end rest'
  in
    concatenate list (joinList rest)

-- In case if we don't want to define Monad instance for (Fun i)

-- bind :: Fun i a -> (a -> Fun i b) -> Fun i b
-- bind (F f) g = F $ \i -> z i i where
--   z i = x where
--     F x = g (f i)

-- joinFun :: Fun i (Fun i a) -> Fun i a
-- joinFun fun = bind fun id -- joinFun = flip bind id (? didn't try ?)

instance Functor (Fun i) where
  fmap = mapFun

instance Applicative (Fun i) where
  pure = wrapFun
  (<*>) = ap

-- instance Monad (Fun i) where
--   (F f) >>= g = F $ \i -> z i i where
--     z i = x where
--       F x = g (f i)

instance Monad (Fun i) where
  (F f) >>= g = F $ \i ->
    let F x = g $ f i
    in
      x i

-- Unlike Task 1 and Task 2, according to the text of Task 3, we
-- are free to use what we want (instances of Functor, Applicative, Monad
-- for Fun i, import of functions ap and join from Control.Monad), however,
-- as precaution, expected (nearly) definition of joinFun is also provided:
-- joinFun :: Fun i (Fun i a) -> Fun i a
-- joinFun (F f) = F $ \s ->
--   let F g = f s
--   in
--     g s

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun = join

{-
In order to remember applicative laws it will be
checked here for (Fun i).
1. Identity law: pure id <*> v = v
From GHC/Base.hs:
ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }

Function ap unwraps the first argument, unwraps the second argument and
returns wrapped value of application of unwrapped value of the first
argument to unwrapped value of the second argument.

pure id :: Fun i (a -> a), so v :: Fun i a must be.

Function pure was defined as "pure val = F $ const val".
As const id :: i -> (a -> a), then "x1 <- F $ const id" let us have
(a -> a) in x1, which is id. As "x2 <- F $ i -> a", we have
a in x2. Application id to a gives a and "return a" gives F $ i -> a,
as desired. It looks like application of return value (id) stored in "pure id"
to return value (a) function stored in v and following wrapping of it.

Example in ghci which can help understand how "<-" works for (Fun i):
ghci> F ex = (F $ \x -> [x, x, x]) >>= (\t -> return $ length t)
ghci> ex 10
3
Unwrapped value which goes to t is [x, x, x], not \x -> [x, x, x].

2. Composition law: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
Note: infixl 4 <*>
pure (.) = F $ \i -> h, where h :: (b -> c) -> (a -> b) -> a -> c
u = F $ \i -> h', where h' :: (b -> c)
pure (.) <*> u = F $ \i -> h'', where h'' :: (a -> b) -> a -> c
v = F $ \i -> h''', where h''' :: (a -> b)
pure (.) <*> u <*> v = F $ \i -> h'''', where h'''' :: a -> c
w = F $ \i -> h''''', where h''''' :: a
pure (.) <*> u <*> v <*> w = F $ \i -> h'''''',
where h'''''' :: c

At the same time, looking at the right side of composition law,
we have:
v <*> w = F $ \i -> h, where h :: b
u <*> (v <*> w) = F $ \i -> h', where h' :: c

Example in ghci:
ghci> F x = pure (.) <*> (F $ \i -> (+2)) <*> (F $ \i -> (*10)) <*> (F $ \i -> 12)
ghci> x 10
122
ghci> x 20
122
ghci> F y = (F $ \i -> (+2)) <*> ((F $ \i -> (*10)) <*> (F $ \i -> 12))
ghci> y 3
122
ghci> y 4
122

3. Homomorphism law: pure f <*> pure x = pure $ f x - obviously

4. Interchange law: u <*> pure y = pure ($ y) <*> u
u = F $ \i -> t, so u = pure t, t :: y -> w
We need to check that pure t <*> pure y = pure ($ y) <*> pure t.
pure t <*> pure y gives us pure (t y) by homomorphism law, whereas
pure ($ y) <*> pure t is the same as pure (\f -> f y) <*> pure t, which
is the same as pure $ t y by homomorphism law.
-}
