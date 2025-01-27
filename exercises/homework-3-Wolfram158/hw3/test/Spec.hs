module Main
  ( main
  , spec
  ) where

import Data.Monoid (Sum (..))
import HW3.T1 hiding (($), (.))
import HW3.T2
import HW3.T3
import HW3.T4
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "HW3.T1" $ do
    let check unwrapper mapper f w expected = unwrapper (mapper f w) `shouldBe` expected
    it "mapQuad" $
      check id mapQuad (^(3 :: Integer)) (Q 2 3 4 (5 :: Integer)) (Q 8 27 64 125)
    it "mapList" $ do
      let input = (3 :: Integer) :. 8 :. -2 :. Nil
      check id mapList ((+1) . (^(2 :: Integer))) input (10 :. 65 :. 5 :. Nil)
    it "mapFun" $ do
      let unwrapper x (F f) = f x
      let big = (+31415926)
      let g = big . (*3)
      sequence_ [check (unwrapper x) mapFun big (F (*3)) (g x) | x <- [-200..200 :: Integer]]
  describe "HW3.T2" $ do
    let homo distF wrapF wrapF' a b = distF (wrapF a, wrapF b) == wrapF' (a, b)
        homo' :: Eq a => ((Annotated String a, Annotated String a) -> Annotated String (a, a)) ->
           (a -> Annotated String a) -> ((a, a) -> Annotated String (a, a)) -> a -> a -> Bool
        homo' distF wrapF wrapF' a b = distF (wrapF a, wrapF b) == wrapF' (a, b)
    it "List_dist_homomorphism" $ do
      let list1 = (3 :: Integer) :. 6 :. Nil
      let list2 = 10 :. 8 :. 20 :. Nil
      let actual = homo distList wrapList wrapList list1 list2
      actual `shouldBe` True
    it "Annotated_dist_homomorphism" $ do
      let ann1 = (Sum (3 :: Integer), Sum (7 :: Integer)) :# "php"
      let ann2 = (Sum 11, Sum 9) :# "fft"
      let actual = homo' distAnnotated wrapAnnotated wrapAnnotated ann1 ann2
      actual `shouldBe` True
  describe "HW3.T3" $ do
    it "Fun_join" $ do
      let F f = joinFun (F $ \i -> F $ \j -> 2 * i - 3 * i * j + j * j)
      let g x = 2 * (x - x * x)
      sequence_ [f x `shouldBe` g x | x <- [-200..200 :: Integer]]
    it "Fun_Applicative" $ do
      let F f = (.) <$> pure (\j -> j * j - 3 * j + 7) <*> pure (\j -> j * j + 1)
      let g x = y * y - 3 * y + 7 where
                y = x * x + 1
      let f' = f id
      sequence_ [f' x `shouldBe` g x | x <- [-200..200 :: Integer]]
    it "Fun_Monad_01" $ do
      let F f = (F $ \i -> i * i) >>= \t -> return $ t * t + 7 * t - 1
      let g x = x * x * x * x + 7 * x * x - 1
      sequence_ [f x `shouldBe` g x | x <- [-200..200 :: Integer]]
    it "Fun_Monad_02" $
      let f x = do
            g0 <- F $ \_ -> x + 3
            g1 <- F $ \_ -> 2 * g0 + 1
            F $ \_ -> g1 * g1 + 2
      in
        do
          let g x = 4 * x * x + 28 * x + 51
          sequence_ [let F f' = f x in f' id `shouldBe` g x | x <- [-200..200 :: Integer]]
    it "Fun_Monad_02'" $
    -- hlint recommends:
    -- Suggestion: Use let
    -- Found:
    -- do g0 <- pure $ x + 3
    --    g1 <- pure $ 2 * g0 + 1
    --    pure $ g1 * g1 + 2
    -- Perhaps:
    -- do let g0 = x + 3
    --    let g1 = 2 * g0 + 1
    --    pure $ g1 * g1 + 2
      let f x = do
            g0 <- pure $ x + 3
            g1 <- pure $ 2 * g0 + 1
            pure $ g1 * g1 + 2
      in
        do
          let g x = 4 * x * x + 28 * x + 51
          sequence_ [let F f' = f x in f' id `shouldBe` g x | x <- [-200..200 :: Integer]]
    it "List_join" $ do
      let list1 = "using" :. "namespace" :. "std" :. Nil
      let list2 = "from" :. "scipy" :. "import" :. "optimize" :. Nil
      let list3 = "#" :. Nil
      let input = list1 :. list2 :. list3 :. Nil
      let expected = "using" :. "namespace" :. "std" :. "from" :. "scipy"
                     :. "import" :. "optimize" :. "#" :. Nil
      joinList input `shouldBe` expected
      let list2' = Nil
      let input1 = list1 :. list2' :. list3 :. Nil
      let expected1 = "using" :. "namespace" :. "std" :. "#" :. Nil
      joinList input1 `shouldBe` expected1
    it "Annotated_join_left_id" $ do
      let ann = (235236 :: Integer) :# "bdfgdbc"
      joinAnnotated (wrapAnnotated ann) `shouldBe` ann
    it "Annotated_join_right_id" $ do
      let ann = "sfdbdfg" :# "1"
      joinAnnotated (mapAnnotated wrapAnnotated ann) `shouldBe` ann
    it "Annotated_join_associativity" $
      let testAnnotatedJoinAssociativity a b c d e =
            joinAnnotated (mapAnnotated joinAnnotated ann) ==
              joinAnnotated (joinAnnotated ann) where
                ann = (((a :# b) :# c) :# d) :# e
      in
        do
          testAnnotatedJoinAssociativity "" "2fgd" "dfgj" "c" "" `shouldBe` True
          testAnnotatedJoinAssociativity "x" "yz" "tf" "vdf" "" `shouldBe` True
          testAnnotatedJoinAssociativity "v" "2f" "" "" "" `shouldBe` True
          testAnnotatedJoinAssociativity "zz" "2fgd" "dfgj" "c" "" `shouldBe` True
          testAnnotatedJoinAssociativity "vdg" "2fgd" "dfgj" "c" "" `shouldBe` True
          testAnnotatedJoinAssociativity "" "" "dfgj" "c" "" `shouldBe` True
          testAnnotatedJoinAssociativity "123" "2fgd" "321" "cf" "xcv" `shouldBe` True
  describe "HW.T4" $ do
    let help = do
          modifyState (5 :)
          modifyState (2 :)
          modifyState (7 :)
    it "modifyState_01" $
      let res = do
            help
            modifyState (map ((+1) . (^(2 :: Integer))))
      in
        do
          let ann = runS res []
          ann `shouldBe` (() :# [50 :: Integer, 5, 26])
    it "modifyState_02" $
      let res = do
            help
            modifyState (map ((+1) . (^(2 :: Integer))))
            sequence_ [modifyState (x :) | x <- [100..200]]
      in
        do
          let ann = runS res []
          ann `shouldBe` (() :# [200, 199..100 :: Integer] <> [50, 5, 26])
    it "State_Applicative" $ do
      let f :: Integer -> State [Integer] Integer
          f x = (.) <$> S { runS = \_ -> (*2) :# [1, 2, 3] } <*>
            S { runS = \_ -> (+3) :# [5, 1] } <*> S { runS = \_ -> x :# [0] }
      let g x = (2 * x + 6) :# [0]
      sequence_ [runS (f i) [] `shouldBe` g i | i <- [-200..200 :: Integer]]
    it "eval" $
      let evaled = do
            x <- eval $ Op $ Mul (Op $ Add (Val 3) (Val 5))
                                 (Op $ Sub (Op $ Abs (Val (-3))) (Val 11))
            y <- eval $ Op $ Div (Op $ Sub (Val 11) (Val 5)) (Val 2)
            z <- eval $ Op $ Sgn (Op $ Sub (Val 2718281828459045) (Val 2718281828459048))
            return $ 4 * x + 120 * y - 124 * z
      in
        runS evaled [] `shouldBe` (228 :# [Sgn (-3), Sub 2718281828459045 2718281828459048,
                                          Div 6 2, Sub 11 5, Mul 8 (-8), Sub 3 11, Abs (-3),
                                          Add 3 5])

main :: IO ()
main = hspec spec
