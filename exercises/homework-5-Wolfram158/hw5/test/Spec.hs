module Main
  ( main
  , spec
  ) where

import Control.Monad.Trans (liftIO)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import HW5.Action (HIO (..), HiPermission (..))
import HW5.Base (HiError (..), HiExpr (..), HiFun (..), HiValue (..))
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import Test.Hspec
-- import Numeric.Natural
-- import Data.Char (chr)
-- import System.Random

-- randomString :: Natural -> IO String
-- randomString 0 = randomRIO (97, 123) >>= return . show . chr
-- randomString n =
--     do
--         str <- randomString $ pred n
--         ch <- randomRIO (97, 123)
--         return $ (chr ch) : str

spec :: SpecWith ()
spec = do
  let check1 input err =
        case parse input of
            Left _ -> "String was parsed" `shouldBe` "String was not parsed"
            Right expr ->
                do
                    res <- liftIO $
                             runHIO (eval expr) $
                               Set.fromList [AllowRead, AllowWrite, AllowTime]
                    case res of
                        Left e  -> e `shouldBe` err
                        Right _ -> "HiError was not thrown" `shouldBe` "HiError was thrown"
  let check2 input val =
        case parse input of
            Left _ -> "String was parsed" `shouldBe` "String was not parsed"
            Right expr ->
                do
                    res <- liftIO $
                             runHIO (eval expr) $
                               Set.fromList [AllowRead, AllowWrite, AllowTime]
                    case res of
                        Right r -> r `shouldBe` val
                        Left _  -> "HiError was thrown" `shouldBe` "HiError was not thrown"
  let check3 input boo =
          case parse input of
            Left _ ->
              if boo
                then "String was not parsed" `shouldBe` "String was parsed"
                else True `shouldBe` True
            Right _ ->
              if boo
                then True `shouldBe` True
                else "String was parsed" `shouldBe` "String was not parsed"
  describe "HW5.T1" $ do
    it "parse_01" $
      let
        app = HiExprValue $ HiValueFunction HiFunAdd
        args = [HiExprValue $ HiValueNumber 500,
                HiExprValue $ HiValueNumber $ -12]
        expected = HiExprApply app args
      in
        parse "add(500, -12)" `shouldBe` Right expected
    it "parse_02" $
      let
        app = HiExprApply (HiExprValue $ HiValueFunction HiFunDiv) args1
        args1 = [HiExprValue $ HiValueNumber 1]
        args = [HiExprValue $ HiValueNumber 0]
        expected = HiExprApply app args
      in
        parse "((div)(1))(0)" `shouldBe` Right expected
    it "eval_01" $ check1 "((div)(1))(0)" HiErrorArityMismatch
    it "eval_02" $ check2 " ( add )( 1 , mul ( div(5,sub(4,14)) , - 5 ))" $ HiValueNumber 3.5
    it "eval_03" $ check1 "mul(div,div(1,0))" HiErrorDivideByZero
  describe "HW5.T2" $ do
    it "eval_01" $
      let
        input = "if(less-than(mul(3,2),mul(0.5,13)),add,not)(not(true),and(true,not(false)))"
      in
        check1 input HiErrorInvalidArgument
    it "eval_02" $
      let
        input = "if(not(less-than(-1,add(-3,-10))),if(false,add,mul),div(1,0))(200,1.5)"
      in
        check2 input $ HiValueNumber 300
  describe "HW5.T3" $ do
    it "eval_01" $ check2 " sub ( -3, mul ( -12 , 3 ) ) * (1 + 2) / 8 + 1 " (HiValueNumber 13.375)
    it "eval_02" $
      let
        input = "((if)(1<=2,if(not(1==1),1/2,-3*mul(1.5,3)),-2))(if(1/=1||true,mul,div)(2,3))(5)"
      in
        check1 input HiErrorInvalidFunction
    it "eval_03" $
      let
        input = "-   3 * (1 + 7)*( 2  - 7 * 5) / (7 + 2 * 2) * (1 - 1 / 2) + 1 "
      in
        check2 input (HiValueNumber 37)
    it "eval_04" $
      let
        input =
            "not(true) || 1 >= 3 && 2/= 2 || (false || false) || if(not(false),true&&true, false)"
      in
        check2 input $ HiValueBool True
  describe "HW5.T4" $ do
    it "eval_01" $ check2 "(\"sfddsfg\" + \"fft\")(2,9)" $ HiValueString $ T.pack "ddsfgff"
    it "eval_02" $
      let
        input = "1 + length(\"cabal\" + \" i\nstall \" + to-upper(\"ra\ndom\")) * 2"
      in
        check2 input $ HiValueNumber 41
    it "eval_03" $
      let
        input1 = "\"cpp\n\"(3) /= \"notes\n\t\"(5) || \"has\"*3 + \"kel\""
        input2 = "== reverse(\"leksahsahsah\")"
        input = input1 <> input2
      in
        check2 input $ HiValueBool True
    it "eval_04" $
      let
        input = " to-upper(\"Ocaml\") + to-lower(\"cameL\")(4) * 2 + reverse(\"++g\")(1,2)*3"
      in
        check2 input $ HiValueString $ T.pack "OCAMLll+++"
  describe "HW5.T5" $ do
    it "eval_01" $
      let
        input = "length([1,true,null])/5 + (length)(reverse([1,2]*3+[5,6])(3,100))"
      in
        check2 input $ HiValueNumber 5.6
    it "eval_02" $
      let
        input = "fold(if(true, add, mul(1)), range(-100.3, 228.1)) / (1 + 164 * 2)"
      in
        check2 input $ HiValueNumber 63.7
    it "list_01" $ check3 " [ \"elliptic\n\" , [# 63 75 72 76 65 20 6d 65 74 68 6f 64 #] ]" True
    it "list_02" $ check3 "[1,2,true,[3,[5,[6],[[[8]]]]],9,\"grad\"]" True
    it "list_03" $ check3 " [    1 + 2 * length([1,2,[3]]) ,  7 - 0.123 , fold()()() ] " True
    it "list_04" $ check3 "[]" True
    it "list_05" $ check3 "[    ]" True
  describe "HW5.T6" $ do
    it "bytes_01" $ check3 "[# #]" True
    it "bytes_02" $ check3 "\n[\n# 10 20 30 # ]" False
    it "bytes_03" $ check3 "[\n\t#1e 2f\n#]" False
    it "bytes_04" $ check3 "[# 2a 3b 01 02 #]" True
    it "bytes_05" $ check3 "[# 2a 1e 0 #]" False
    it "bytes_06" $ check3 "[# 1f fc fe faab #]" False
    it "bytes_07" $ check3 "[# 10 11 12 13 0 #]" False
    it "bytes_08" $ check3 "[#10 20 30 40 50#]" True
    it "bytes_08" $ check3 "\n \n[\n\t # \t\n10 20 \t\n30 40 \n50 \t\t# \n] " False
    it "bytes_09" $ check3 " [ # 1f 2e ff ee aa a0 01 0a # ] " False
    it "bytes_10" $ check3 "[# 10 11 12 13 0g #]" False
    it "pack-bytes_01" $ check1 "pack-bytes([3, 256, 158, 32])" HiErrorInvalidArgument
  -- describe "HW5.T7" $ do
  --   it "session_01" $
  --     do
  --       tmp <- liftIO $ randomString 16
  --       check2 ("mkdir(\"" <> tmp <> "\")!") HiValueNull
  --       check2 ("read(\"" <> tmp <> "\")!") $ HiValueList $ Seq.empty
  --       check2 ("mkdir(\"" <> tmp <> "/a\")!") HiValueNull
  --       check2 ("mkdir(\"" <> tmp <> "/b\")!") HiValueNull
  --       check2 ("read(\"" <> tmp <> "\")!") $
  --         HiValueList $
  --           Seq.fromList [HiValueString $ T.pack "a", HiValueString $ T.pack "b"]
  --       check2 ("write(\"" <> tmp <> "/hi.txt\", \"Hello\")!") $ HiValueNull
  --       check2 ("cd(\"" <> tmp <> "\")!") HiValueNull
  --       check2 "read(\"hi.txt\")!" $ HiValueString $ T.pack "Hello"
  describe "HW5.T11" $ do
    it "dict_01" $ check3 "{1:2, 3:4, }" False
    it "dict_02" $ check2 "{1:true,\"a2\":false}.a2" $ HiValueBool False
    it "dict_03" $
      let
        input = " { \"a1\" : [ 1 , 2 , 3 ] , true : false , 5 : { } }.a1(1,null)(1)*17+3"
      in
        check2 input $ HiValueNumber 54
  describe "Extra" $ do
    it "test_01" $ check2 " - ( 1 + 3 * ( 1 - ( - 1) * 2 ) ) / 5 + 3 " $ HiValueNumber 1
    it "test_02" $ check2 " -length(\"length\"(1,3)/\"jmpaux\"(2,7))*(-3)" $ HiValueNumber 21
    it "test_03" $ check2
                     " -if(null==null,if(true,add,add(3)),(mul))(1,mul(2,3))" $
                       HiValueNumber $ -7
    it "test_04" $ check2
                     " - ( (-length(reverse(\"xyz\")(1,null)) + length(\"int\"(1)))*2*3*4 )" $
                       HiValueNumber 24
    it "test_05" $ check2 "-if(false/=true,-fold(mul,range(1,5)),-3)" $ HiValueNumber 120
    it "test_06" $ check1 "range([1,5])" HiErrorArityMismatch
    it "test_07" $ check2 "(reverse(range(1+3,10*2))+(range(50,100)*2)(1,2))(7,-3)" $
                     HiValueList $ Seq.fromList $ HiValueNumber <$> [13, 12..6]
    it "test_08" $ check1 "range(1,2(3))" HiErrorInvalidFunction
    it "test_09" $
      let
        input1 = " - ( length((\"s\"*length ( to-lower ( reverse "
        input2 = "( to-upper ( \"gf(2)\" ) ) ) ) * 2 "
        input3 = " + \"a\"*3 * ( length(( trim( \" gmpxx.h \" ) + reverse(\"\")(1,5) ))))(1,8)) )"
      in
        check2 (input1 <> input2 <> input3) $ HiValueNumber $ -7
    it "test_10" $ check2 "\n" HiValueNull
    it "test_11" $
      let
        input1 = " not( true ) || if(not(less-than(false,true)), "
        input2 = "not-less-than(1 + 2, - (1+4*(-100))),1)"
      in
        check2 (input1 <> input2) $ HiValueNumber 1
    it "test_12" $ check3 "cwd!!" True
    it "test_13" $ check3 "now!-now!" True
    it "test_14" $ check3 "cd(\"tmp\")!" True
    it "test_15" $ check3 "cd(\"tmp\")!!" True
    it "test_16" $ check3 "cd(\"tmp\")!!!" True
    it "test_17" $ check2 "(((add))(1,2))+(if((true),(mul),div))(5,6)" $ HiValueNumber 33
    it "test_18" $ check3 "add(1,)" False
    it "test_19" $ check3 "if(1,2,3,)" False
    it "test_20" $ check3 "add()" True
    it "test_21" $
      let
        input1 = "(-(1)-((2))*(3+(4-length.xyz)))*(-(1))+((length)((\"l\")))"
      in
        check2 input1 $ HiValueNumber 10
    it "test_22" $ check2 "1+2*fold(add,keys(count(values({1:true,2:1,{}:[1,2],[##]:\"\"})(3))))" $
      HiValueNumber 7
    it "test_23" $ check2 "if(1=={\"x\":1}.x,fold,fold())(mul,range(1,5))" $ HiValueNumber 120
    it "test_24" $ check2 "[{null:[1,2,3]}, null, {\"null\":[false,true]}](2).null(0) || true" $
      HiValueBool True
    it "test_25" $ check2 "if(not-less-than(0,0),1,2)" $ HiValueNumber 1

main :: IO ()
main = hspec spec
