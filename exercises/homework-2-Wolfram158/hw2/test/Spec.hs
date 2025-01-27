module Main
  ( main
  , spec
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import HW2.T1
import HW2.T2
import HW2.T3
import HW2.T4
import Test.Hspec

newtype Wrapper a = Value a deriving (Show, Eq)

instance Num a => Semigroup (Wrapper a) where
  Value x <> Value y = Value (x + y)

instance Num a => Monoid (Wrapper a) where
  mempty = Value 0

spec :: SpecWith ()
spec = do
  let mkTree [] = Leaf
      mkTree elems = Branch 1 tleft val tright where
        ind = length elems `div` 2
        tleft = mkTree $ take ind elems
        right = drop ind elems
        val = head right
        tright = mkTree (tail right)
  describe "HW2.T1" $ do
    it "tfoldr_test01" $ do
      let tree = mkTree ([1..100] :: [Int])
      let summary = tfoldr (+) 0 tree
      summary `shouldBe` 5050
    it "tfoldr_test02" $ do
      let tree = mkTree ([1..9] :: [Int])
      let summary = tfoldr (*) 1 tree
      summary `shouldBe` 362880
    it "foldable_test01" $ do
      let tree = mkTree ([Value x | x <- [1..10]] :: [Wrapper Int])
      let summary = foldMap (\(Value x) -> Value (x * x)) tree
      summary `shouldBe` Value 385
  describe "HW2.T2" $ do
    it "splitOn_test01" $ do
      let prefix = "Microsoft.PowerShell.Core\\FileSystem::"
      let suffix = "\\wsl$\\Ubuntu-20.04\\root\\haskell-hw\\homework-2-Wolfram158\\"
      let input = prefix <> suffix
      let expected1 = "Microsoft.PowerShell.Core" :| ["FileSystem::"]
      let expected2 = "wsl$" :| ["Ubuntu-20.04", "root", "haskell-hw"]
      let expected3 = "homework-2-Wolfram158" :| [""]
      let expected = expected1 <> expected2 <> expected3
      splitOn '\\' input `shouldBe` expected
    it "splitOn_test02" $ do
      let input = ".problems..output..debug.console.terminal...ports.."
      let result = splitOn '.' input
      let expected1 = "" :| ["problems", "", "output", "", "debug"]
      let expected2 = "console" :| ["terminal", "", ""]
      let expected3 = "ports" :| ["", ""]
      let expected = expected1 <> expected2 <> expected3
      result `shouldBe` expected
    it "splitOn_test03" $ do
      let input = [1, 1, 2, 3, 4, 5, 6, 1, 2, 3, 1] :: [Int]
      splitOn 1 input `shouldBe` [] :| [[], [2, 3, 4, 5, 6], [2, 3], []]
    it "joinWith_test01" $ do
      let input = "java" :| ["util", "Scanner"]
      joinWith '.' input `shouldBe` "java.util.Scanner"
    it "joinWith_test02" $ do
      let input = "return" :| []
      joinWith ',' input `shouldBe` "return"
    let identity sep = joinWith sep . splitOn sep
    it "law_test01" $ do
      let input = "l..dfgsertb34..s.ergb..awer34.bte."
      let sep = '.'
      identity sep input `shouldBe` input
    it "law_test02" $ do
      let input = "info/kgeorgiy/ja/*/arrayset"
      let sep = '/'
      identity sep input `shouldBe` input
    it "law_test03" $ do
      let input = "info/kgeorgiy/ja/*/arrayset"
      let sep = '.'
      identity sep input `shouldBe` input
    it "law_test04" $ do
      let input = "/path/to/file//"
      let sep = '/'
      identity sep input `shouldBe` input
  describe "HW2.T3" $ do
    it "mcat_test01" $ mcat ([] :: [Maybe (Wrapper Int)]) `shouldBe` Value 0
    it "mcat_test02" $ do
      let input = [Nothing, Just (Value 9), Nothing, Just (Value 8)]
      mcat (input :: [Maybe (Wrapper Int)]) `shouldBe` Value 17
    it "mcat_test03" $ do
      let input = [Nothing]
      mcat (input :: [Maybe (Wrapper Int)]) `shouldBe` Value 0
    it "epart_test01" $ do
      let input1 = [Left "liftA2", Right (Value 11), Left "liftM2"]
      let input2 = [Right (Value 17), Right (Value 19), Left "liftAM2"]
      let input = (input1 :: [Either String (Wrapper Int)]) <> input2
      epart input `shouldBe` ("liftA2liftM2liftAM2", Value 47)
  describe "HW2.T4" $ do
    it "Inclusive_associativity" $ do
      let left1 = (This "ab" <> This "ghc") <> This "hru"
      let right1 = This "ab" <> (This "ghc" <> This "hru")
      (left1 :: Inclusive String String) `shouldBe` right1

      let left2 = (This "ab" <> This "ghc") <> That "hru"
      let right2 = This "ab" <> (This "ghc" <> That "hru")
      left2 `shouldBe` right2

      let left3 = (This "ab" <> That "ghc") <> This "hru"
      let right3 = This "ab" <> (That "ghc" <> This "hru")
      left3 `shouldBe` right3

      let left4 = (That "ab" <> This "ghc") <> This "hru"
      let right4 = That "ab" <> (This "ghc" <> This "hru")
      left4 `shouldBe` right4
    it "ListPlus_associativity" $ do
      let left1 = Last "x^3" <> (("+" :+ Last "y^3=") <> Last "z^3")
      let right1 = (Last "x^3" <> ("+" :+ Last "y^3=")) <> Last "z^3"
      left1 `shouldBe` right1

      let left22 = ("(x-" :+ Last "yi)=") <> ("x^2" :+ Last "+y^2")
      let left2 = ("(x+" :+ Last "yi)*") <> left22
      let left21 = ("(x+" :+ Last "yi)*") <> ("(x-" :+ Last "yi)=")
      let right2 = left21 <> ("x^2" :+ Last "+y^2")
      left2 `shouldBe` right2

main :: IO ()
main = hspec spec
