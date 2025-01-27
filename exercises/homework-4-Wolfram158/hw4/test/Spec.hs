{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  , spec
  ) where

import Control.Exception (ErrorCall (..), evaluate)
import Exercise hiding (main)
import HW4.T1
import HW4.T2 (parseExpr)
import HW4.Types
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "LogicNotPush" $ do
    let check f samples expected =
          sequence_ [f sample `shouldBe` expected | sample <- samples]
    it "Case 1" $ do
      let samples =
            [
              "~(a|b)", " ~ (a |b)", "~(     a|b)   ", "~ ( a | b ) ",
              "~a&~b", "~a & ~b", " ~ a & ~ b ", "~(~(~(~(~a)))|~(~(b)))"
            ]
      check push samples "~a & ~b"
    it "Case 2" $ do
      let samples =
            [
              "~(a|~b)", " ~ (a |~b)", "~(     a|~(~(~b)))   ", "~ ( a | ~b ) ",
              "~a & b", "~(~(~a)|~((b)) )", " ~ ( ~ ( ~ a ) )  & ( ( b ) ) "
            ]
      check push samples "~a & b"
    it "Case 3" $ do
      let samples =
            [
              "~((( ~ a))|0 & x)", "~((( ~ (((~(~a))))))|0 & x)",
              " ~ ( ( (   ~  a ) )|  ~ ( ~ ( ~ 1 ) ) & ( x ) )",
              " ~ ( ( (   ~  a ) ) |  ~ ( ~ ( ~ 1 ) ) & ( x ) )"
            ]
      check push samples "a & (1 | ~x)"
    it "Case 4" $ push "~(a|~(b|0))&c" `shouldBe` "~a & (b | 0) & c"
    it "Case 5" $ do
      let bads =
           [
            "~()", "~", "a|4", "a b | c & d", "var & 1", "~~x", "a & b | c    v",
            " ~a | ~~(b)", "~x|~y&&0", "x|~|y", "~1&~2", "~(a|b", "~a|b)"
           ]
      check (take 5 . push) bads "Error"
  let th = \case
        ErrorCall "DivideByZero" -> True
        _                        -> False
  let calc' es = case runES es [] of
        Error x          -> error $ show x
        Success (a :# _) -> a
  describe "HW4.T2" $ do
    let getES input = case parseExpr input of
          Success x -> eval x
          Error y   -> error $ show y
    let calc = calc' . getES
    let verify input expected = calc input `shouldBe` expected
    it "One positive number" $ verify "   3.93 " 3.93
    it "One negative number" $ verify "  - 2.29 " $ -2.29
    it "Sum_01" $ verify " 3+  100.227 " 103.227
    it "Sum_02" $ verify "-2+1.2" $ -0.8
    it "Mul_01" $ verify "-3*22" $ -66
    it "Mul_02" $ verify "22*(-9)" $ -198
    it "Div_01" $ verify "5 / -8" $ -0.625
    it "Div_02" $ verify " -22 / 4" $ -5.5
    it "Combination_01" $ verify " -(- (3)) * 22 / 5 * 4 - 3 * 9 * 2*52.8/54" 0
    it "Combination_02" $ verify "-(-2+5*2)*( 19 - 8 * (11 - 3) ) / 4.5 + 0.0" 80
    it "Combination_03" $ verify "1+2*(3+4)" 15
    it "Combination_04" $ verify "(1-2)*(3+4*5-1*7)" $ -16
    it "Combination_05" $ verify "1/2*5 *1/2*(1-3) " $ -2.5
    it "Combination_06" $ verify "-(1/2+3)/4/5*20+123" 119.5
    it "Combination_07" $ verify "1+2*(3+4*(5+6*(7+8)))" 767
    it "Combination_08" $ verify "1/(2+3)/((- 0.2)*5)*5+11" 10
    it "Combination_09" $ verify "2+2+8-22*8" $ -164
    it "Combination_10" $ verify "0.0000*3.141592600+2.71828-0.7182800" 2
    it "Combination_11" $ verify "\n 10 + 3 \t * 11 \n - 9 * 2 \n \t +   3  \n" 28
    it "Minuses_01" $ verify "1-1-1" $ -1
    it "Minuses_02" $ verify "1-2-3-4-5" $ -13
    it "Uncaught case" $ verify "( ( 3 ) )" 3
    it "Bads" $ do
      let bads =
           [
            "", "-", " ", "+", "-3+", "2*", "1+(2*)3", "1+.3", ".2", "3.", "1+3.", "(6+1",
            "/5", "+-2", "3.0 5.4", "2+3*x-9", "()", "(((1) + 2)-3))", "1.. + 3"
           ]
      sequence_ [evaluate (calc input) `shouldThrow` anyException | input <- bads]
    it "Division by zero" $ evaluate (calc "(1+2)/(3*5-2*7.5+1-1)") `shouldThrow` th
  describe "HW4.T1" $ do
    let calc'' es = case runES es [] of
          Error x          -> error $ show x
          Success (_ :# s) -> s
    it "eval" $ let expected = [Sub 1.75 8, Mul 4 2, Add 1 0.75, Div 6 8, Mul 2 3] in
      calc'' (eval $ 1 + 2 * 3 / 8 - 4 * 2) `shouldBe` expected
    it "Division by zero" $
      evaluate (calc'' $ eval $ 1 - 2 + 3 - 4 + 5 / (1 - 2 + 3 - 2)) `shouldThrow` th
    it "Monad_ExceptState" $ do
      let sth = do
            x <- eval $ 1 + 2 * 3
            modifyExceptState $ (:) $ Sub 227 229
            y <- eval $ Val x * 3
            z <- eval $ Val y / 14
            return $ z + 100
      let expected = [Div 21 14, Mul 7 3, Sub 227 229, Add 1 6, Mul 2 3]
      calc'' sth `shouldBe` expected
      calc' sth `shouldBe` 101.5

main :: IO ()
main = hspec spec
