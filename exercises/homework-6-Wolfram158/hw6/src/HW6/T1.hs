{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  , getChtDetails

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (MonadConc, STM, atomically, newTVar, readTVarConc)
import Control.Concurrent.Classy.STM (MonadSTM, TArray, TVar)
import Control.Concurrent.Classy.STM.TVar (modifyTVar, readTVar, writeTVar)
import Control.Lens (_3, view)
import Control.Monad (foldM, forM_, when, (>=>))
import Data.Array.Base (MArray, getNumElements)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Hashable (Hashable (hash))

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

getIndex :: Hashable k => k -> Int -> Int
getIndex = mod . hash

newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT =
  atomically $
    do
      size <- newTVar 0
      buckets <- newArray (0, pred initCapacity) [] >>= newTVar
      return CHT {
        chtBuckets = buckets,
        chtSize = size
      }

getCHT
  :: ( MonadConc m
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT key cht =
  atomically $ lookup key . view _3 <$> getChtDetails key cht

-- Based on constraints for types suggested by GHC for functions
-- getChtDetails and redoubleCapacity
type UtilsConstraint stm m k v =
  ( TVar stm ~ TVar m
  , MonadSTM m
  , MArray (TArray stm) (Bucket k v) m
  , Hashable k
  )

getChtDetails
  :: UtilsConstraint stm m k v
  => k
  -> CHT stm k v
  -> m (BucketsArray stm k v, Int, Bucket k v)
getChtDetails key cht =
  do
    bucketsArray <- readTVar $ chtBuckets cht
    capacity <- getNumElements bucketsArray
    pairs <- readArray bucketsArray $ getIndex key capacity
    return (bucketsArray, capacity, pairs)

putCHT
  :: ( MonadConc m
     , Hashable k
     )
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT key value cht =
  atomically $
    do
      (bucketsArray, capacity, pairs) <- getChtDetails key cht
      let mfolder acc pair@(key', _)
            | key' == key = modifyTVar (chtSize cht) pred >> return acc
            | otherwise = return $ pair:acc
      clarification <- foldM mfolder [] pairs
      writeArray bucketsArray (getIndex key capacity) $ (key, value):clarification
      modifyTVar (chtSize cht) succ
      size <- readTVar $ chtSize cht
      when (fromIntegral size >= loadFactor * fromIntegral capacity) $
        redoubleCapacity capacity cht

redoubleCapacity
  :: UtilsConstraint stm m k v
  => Int
  -> CHT stm k v
  -> m ()
redoubleCapacity capacity cht =
  do
    let doubledCapacity = 2 * capacity
    new <- newArray (0, pred doubledCapacity) []
    old <- readTVar $ chtBuckets cht
    forM_ [0 .. pred capacity] $
      readArray old >=>
        mapM_ (
          \pair@(key', _) ->
            do
              let index = getIndex key' doubledCapacity
              currentPairs <- readArray new index
              writeArray new index $ pair:currentPairs
                )
    writeTVar (chtBuckets cht) new

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT = readTVarConc . chtSize
