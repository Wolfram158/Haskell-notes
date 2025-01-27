module Main
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import HW1.T1
import HW1.T2
import HW1.T3
import Numeric.Natural
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "HW1.T1" $ do
      it "afterDays_test01" $ afterDays 48 Thursday `shouldBe` Wednesday
      it "afterDays_test02" $ afterDays 137 Friday `shouldBe` Tuesday
      it "afterDays_test03" $ afterDays 113 Saturday `shouldBe` Sunday
      it "daysToParty_test01" $ daysToParty Sunday `shouldBe` 5
    describe "HW1.T2" $ do
      let one = S Z
      let two = S one
      let three = S two
      let four = S three
      let six = S (S (S three))
      it "nplus_test01" $ nplus Z two `shouldBe` two
      it "nplus_test02" $ nplus three Z `shouldBe` three
      it "nplus_test03" $ nplus two one `shouldBe` three
      it "nmult_test01" $ nmult Z two `shouldBe` Z
      it "nmult_test02" $ nmult one Z `shouldBe` Z
      it "nmult_test03" $ nmult three two `shouldBe` six
      it "ndiv_test01" $ ndiv six two `shouldBe` three
      it "ndiv_test02" $ ndiv two six `shouldBe` Z
      it "ndiv_test03" $ evaluate (ndiv one Z) `shouldThrow` anyException
      it "nsub_test01" $ nsub one three `shouldBe` Nothing
      it "nsub_test02" $ nsub six two `shouldBe` Just four
      it "ncmp_test01" $ ncmp six two `shouldBe` GT
      it "ncmp_test02" $ ncmp one two `shouldBe` LT
      it "ncmp_test03" $ ncmp six six `shouldBe` EQ
      it "nFromNatural_test01" $ nFromNatural 9 `shouldBe` nplus three six
      it "nFromNatural_test02" $ nFromNatural 6 `shouldBe` six
      it "nFromNatural_test03" $ nFromNatural 24 `shouldBe` nmult four six
      it "nToNum_test01" $ nToNum six `shouldBe` (6 :: Integer)
      it "nToNum_test02" $ do
        let twoOneSix = nmult (nmult six six) six
        nToNum twoOneSix `shouldBe` (216 :: Natural)
      it "nToNum_test03" $ do
        let oneSix = foldr nplus Z [one, two, three, four, six]
        nToNum oneSix `shouldBe` (16 :: Double)
      {-
      it "nmod_test01" $ do
        let num65537 = S $ foldr1 nmult (replicate 16 two) -- foldr1 is non-total function
        -- let num65537 = nFromNatural 65537
        let power = foldr1 (\x y -> (nmult x y) `nmod` num65537) (replicate 65536 two)
        power `shouldBe` one
      -} -- too slow (as expected)
    describe "HW1.T3" $ do
      let right1 = mkBranch Leaf (9 :: Int) Leaf
      let tree1 = mkBranch Leaf (7 :: Int) right1
      it "tsize_test01" $ tsize tree1 `shouldBe` 2
      let tree2 = tinsert 3 tree1
      let left1 = mkBranch Leaf (3 :: Int) Leaf
      it "tinsert_test01" $ tree2 `shouldBe` mkBranch left1 (7 :: Int) right1
      it "tinsert_test02" $ tinsert 3 tree2 `shouldBe` tree2
      let tree3 = tFromList ([5, 3, 1, 2, 4, 3, 7, 8, 9, 6] :: [Int])
      {-
      let left2 = mkBranch Leaf 1 (mkBranch Leaf 2 Leaf)
      let right2r = mkBranch Leaf 8 (mkBranch Leaf 9 Leaf)
      let right2l = mkBranch (mkBranch Leaf 4 Leaf) 5 (mkBranch Leaf 6 Leaf)
      let right2 = mkBranch right2l 7 right2r
      it "tFromList_test01" $ tree3 `shouldBe` mkBranch left2 3 right2
      -}
      --Probably there are several ways to build AVL tree from list of values,
      --so the tree which was built by one visualizer can be not the same as the tree
      --provided by program. Instead we can check that tree is a search tree and
      --is balanced tree.
      let isBalanced Leaf                    = True
          isBalanced (Branch _ left _ right) = pred1 && pred2 && pred3 where
            pred1 = abs (tdepth left - tdepth right) <= 1
            pred2 = isBalanced left
            pred3 = isBalanced right
      let isSearchTree' Leaf _  = True
          isSearchTree' (Branch _ left val right) p = pred1 && pred2 && pred3 where
            pred1 = isSearchTree' left (< val)
            pred2 = isSearchTree' right (> val)
            pred3 = p val
      let isSearchTree = flip isSearchTree' (const True)
      let predicate tree = isSearchTree tree && isBalanced tree
      it "tFromList_test01" $ tree3 `shouldSatisfy` predicate

main :: IO ()
main = hspec spec
