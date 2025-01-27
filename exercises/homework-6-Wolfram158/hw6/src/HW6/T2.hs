{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains _ '[] = False
  Contains x (x : rest) = True
  Contains x (x' : rest) = Contains x rest

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete _ '[] = '[]
  Delete x (x : rest) = rest
  Delete x (x' : rest) = x' : Delete x rest

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add x set = x : Delete x set
