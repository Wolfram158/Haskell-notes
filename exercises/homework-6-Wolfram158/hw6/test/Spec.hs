{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main
  ( main
  , spec
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Classy (atomically)
import Control.Monad (void)
import HW6.T1
import HW6.T2
import Test.Hspec

data Proxy a = Proxy
type ExampleSet1 = '[ "a", "b" ] :: TSet
type ExampleSet2 = '[] :: TSet

spec :: SpecWith ()
spec = do
    describe "HW6.T1" $ do
      it "Put once and then get the same once" $ do
        cht <- newCHT
        putCHT "a" "b" cht
        m <- getCHT "a" cht
        m `shouldBe` (Just "b")
      it "Put twice by the same key the same values and measure size" $ do
        cht <- newCHT
        putCHT "a" "b" cht
        putCHT "a" "b" cht
        size <- sizeCHT cht
        size `shouldBe` 1
      it "Put twice by the same key different values and measure size" $ do
        cht <- newCHT
        putCHT "a" "c" cht
        putCHT "a" "d" cht
        size <- sizeCHT cht
        size `shouldBe` 1
      it "Put twice by different keys the same values and measure size" $ do
        cht <- newCHT
        putCHT "a" "c" cht
        putCHT "b" "c" cht
        size <- sizeCHT cht
        size `shouldBe` 2
      it "Modify hash table from several threads" $ do
        cht <- newCHT
        void $ forkIO $ mapM_ (\x -> putCHT x '1' cht) [1 :: Double .. 40]
        void $ forkIO $ mapM_ (\x -> putCHT x '2' cht) [20 :: Double .. 60]
        void $ forkIO $ mapM_ (\x -> putCHT x '1' cht) [30 :: Double .. 80]
        threadDelay 100000
        size <- sizeCHT cht
        size `shouldBe` 80
      it "Modify hash table from several threads with exception in one" $ do
        cht <- newCHT
        void $ forkIO $ mapM_ (\x -> putCHT x '1' cht) [1 :: Double .. 40]
        void $ forkIO $ mapM_ (\x -> putCHT x '2' cht) [error "InputMismatchException"]
        void $ forkIO $ mapM_ (\x -> putCHT x '1' cht) [30 :: Double .. 80]
        threadDelay 100000
        size <- sizeCHT cht
        size `shouldBe` 80
      it "Value must be found by the same key after redoubling" $ do
        cht <- newCHT
        void $ forkIO $ mapM_ (\x -> putCHT (x + 1000) (show x) cht)
          [1 :: Double .. ((*) $ fromIntegral initCapacity) $ loadFactor * 4.3]
        threadDelay 100000
        (_, capacity, _) <- atomically $ getChtDetails 17 cht
        capacity `shouldBe` 128
        m1 <- getCHT 1017 cht
        m1 `shouldBe` (Just "17.0")
        m2 <- getCHT 1049 cht
        m2 `shouldBe` (Just "49.0")
        m3 <- getCHT 1055 cht
        m3 `shouldBe` Nothing
      it "In case of collision the last put value must be available by key" $ do
        cht <- newCHT
        putCHT 1 "d" cht
        putCHT 1 "с" cht
        putCHT 1 "clojure" cht
        size <- sizeCHT cht
        size `shouldBe` 1
        m <- getCHT (1 :: Double) cht
        m `shouldBe` (Just "clojure")
    describe "HW6.T2" $
      it "Type-level set (it must be compiled)" $ do
        let test1 :: Proxy 'True = Proxy @(Contains "a" ExampleSet1)
        let test2 :: Proxy '[ "b" ] = Proxy @(Delete "a" ExampleSet1)
        let test3 :: Proxy '[ "a", "b" ] = Proxy @(Add "a" ExampleSet1)
        let test4 :: Proxy 'False = Proxy @(Contains "c" ExampleSet1)
        let test5 :: Proxy '[ "c", "a", "b" ] = Proxy @(Add "c" ExampleSet1)
        let test6 :: Proxy '[ "d" ] = Proxy @(Add "d" ExampleSet2)
        let test7 :: Proxy '[ "x", "y" ] = Proxy @(Add "x" (Add "y" '[]))
        const '1' (test1, test2, test3, test4, test5, test6, test7) `shouldBe` '1'

main :: IO ()
main = hspec spec
