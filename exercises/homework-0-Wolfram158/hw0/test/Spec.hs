{-# LANGUAGE LambdaCase #-}

module TestsSpec
  ( spec
  , main
  , fib'
  ) where

import Data.Function (fix)
import HW0.T4
import Numeric.Natural (Natural)
import Test.Hspec

fib' :: Natural -> Natural
fib' = fix (\f a b ->
    \case
        0 -> a
        1 -> b
        n -> f b (a + b) (n - 1)
        ) 0 1

spec :: SpecWith ()
spec = do
    describe "Calculation of Fib(10_000_000) in O(logn)" $
        it "Example which can't be fought fast by O(n)-solution" $
            (fib 10000000 `rem` 998244353) `shouldBe` (374917563 :: Natural)
    describe "Calculation of Fib(200_000) in O(n)" $
        it "" $ (fib' 200000 `rem` 998244353) `shouldBe` (189040980 :: Natural)
    describe "Calculation of Fib(200_000) in O(logn)" $
        it "" $ (fib 200000 `rem` 998244353) `shouldBe` (189040980 :: Natural)

main :: IO ()
main = hspec spec
