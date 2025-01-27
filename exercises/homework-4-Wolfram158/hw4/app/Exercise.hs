{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Exercise
  ( main
  , push
  ) where

import Control.Applicative
import Control.Monad
import Data.Char (isAsciiLower)
import qualified Data.List.NonEmpty as NE (NonEmpty (..), map)
import qualified Data.Map as Map
import HW4.T1 hiding (EvaluationError (..))
import HW4.T2
import HW4.Types

-- Good code is not the point of this exercise

{-
In order to exercise little more the exam task named LogicNotPush
from programming paradigms course taken in the second semester
is solved below.
The task is following: given string representing
boolean expression containing constants 0, 1; variables a, b, c, ..., z;
brackets (, ); operations ~ (logic not), |, &.
Need to transform input string such that it does not contain extra brackets,
operation ~ is applied only to variables and new boolean expression is
equivalent to original boolean expression.
Examples:
(~(a&~b)) -> ~a | b
0|a&~0 -> 0 | a & 1
~(a|~(b|0))&c -> ~a & (b | 0) & c

Grammar used in HW4.T2 can be adapted for this task in the following way:
E -> TE'
E' -> |TE'
E' -> eps
T -> FT'
T' -> &FT'
T' -> eps
F -> n (where n is 0 or 1)
F -> ~n
F -> var (where var is in range a, b, c, ..., z)
F -> ~var
F -> (E)
F -> ~(E)
-}

data Boo a =
    And a a
  | Or a a
  | BoolNeg a
  deriving Show

data BoolExpr =
      BoolVal Bool
    | BoolOp (Boo BoolExpr)
    | Var String

data EvaluationError = UndefinedVariable
  deriving Show

instance Show BoolExpr where
    show (Var x) = x
    show (BoolVal False) = "0"
    show (BoolVal True) = "1"
    show (BoolOp (And o1@(BoolOp Or{}) o2@(BoolOp Or{}))) =
        mconcat ["(", show o1, ")", " & ", "(", show o2, ")"]
    show (BoolOp (And o1@(BoolOp Or{}) a1@(BoolOp And{}))) =
        mconcat ["(", show o1, ")", " & ", show a1]
    show (BoolOp (And a1@(BoolOp And{}) o1@(BoolOp Or{}))) =
        mconcat [show a1, " & ", "(", show o1, ")"]
    show (BoolOp (And var1@(Var _) o1@(BoolOp Or{}))) =
        mconcat [show var1, " & ", "(", show o1, ")"]
    show (BoolOp (And o1@(BoolOp Or{}) var1@(Var _))) =
        mconcat ["(", show o1, ")", " & ", show var1]
    show (BoolOp (And neg1@(BoolOp (BoolNeg _)) o1@(BoolOp Or{}))) =
        mconcat [show neg1, " & ", "(", show o1, ")"]
    show (BoolOp (And o1@(BoolOp Or{}) neg1@(BoolOp (BoolNeg _)))) =
        mconcat ["(", show o1, ")", " & ", show neg1]
    show (BoolOp (And val1@(BoolVal _) o1@(BoolOp Or{}))) =
        mconcat [show val1, " & ", "(", show o1, ")"]
    show (BoolOp (And o1@(BoolOp Or{}) val1@(BoolVal _))) =
        mconcat ["(", show o1, ")", " & ", show val1]
    show (BoolOp (And x y)) = mconcat [show x, " & ", show y]
    show (BoolOp (Or x y)) = mconcat [show x, " | ", show y]
    show (BoolOp (BoolNeg v@(Var _))) = "~" <> show v
    show (BoolOp (BoolNeg v@(BoolVal _))) = "~" <> show v
    show (BoolOp (BoolNeg x)) = mconcat ["~(", show x, ")"]

evalBool :: Map.Map String Bool ->
    BoolExpr ->
        ExceptState EvaluationError [Boo Bool] Bool
evalBool vars = \case
  BoolOp (And x y) -> evalBinaryBool vars x y (&&) And
  BoolOp (Or x y) -> evalBinaryBool vars x y (||) Or
  BoolOp (BoolNeg x) -> evalUnaryBool vars x not BoolNeg
  Var x    -> extractBool vars x
  BoolVal x        -> return x

extractBool :: Map.Map String Bool ->
    String ->
        ExceptState EvaluationError [Boo Bool] Bool
extractBool vars x = case Map.lookup x vars of
    Nothing -> throwExceptState UndefinedVariable
    Just y  -> return y

evalBinaryBool :: Map.Map String Bool ->
    BoolExpr -> BoolExpr ->
        (Bool -> Bool -> Bool) ->
             (Bool -> Bool -> Boo Bool) ->
                ExceptState EvaluationError [Boo Bool] Bool
evalBinaryBool vars x y fun ctor = do
  left <- evalBool vars x
  right <- evalBool vars y
  modifyExceptState $ (:) $ ctor left right
  return $ fun left right

evalUnaryBool :: Map.Map String Bool ->
    BoolExpr ->
        (Bool -> Bool) ->
            (Bool -> Boo Bool) ->
                ExceptState EvaluationError [Boo Bool] Bool
evalUnaryBool vars x fun ctor = do
  val <- evalBool vars x
  modifyExceptState $ (:) $ ctor val
  return $ fun val

-- ctors :: Map.Map Char (a -> a -> Boo a)
-- ctors = Map.fromList [('&', And), ('|', Or)]

pE :: Bool -> Parser BoolExpr
pE neg = do
  pSpaces
  left <- pT
  if neg
    then BoolOp . BoolNeg <$> pE' left
    else pE' left

pE'T' :: Parser BoolExpr ->
  (BoolExpr -> Parser BoolExpr) ->
      NE.NonEmpty (Char, BoolExpr -> BoolExpr -> Boo BoolExpr) ->
        BoolExpr ->
          Parser BoolExpr
pE'T' p1 p2 pairs expr = foldr1 (<|>) (NE.map genP pairs) <|> return expr where
  pStripChar char = pSpaces >> pSatisfyEq char >> pSpaces
  genP (char, ctor) = pStripChar char >> p1 >>= p2 . BoolOp . ctor expr
-- pE'T' p1 p2 parser expr = do
--   pSpaces
--   sgn <- parser
--   pSpaces
--   case sgn of
--     Just sign -> do
--       right <- p1
--       case Map.lookup sign ctors of
--         Just ctor -> p2 $ BoolOp $ ctor expr right
--         Nothing   -> empty
--     Nothing -> pure expr

pE' :: BoolExpr -> Parser BoolExpr
pE' expr = pE'T' pT pE' (('|', Or) NE.:| []) expr <* pSpaces
-- pE' = pE'T' pT pE' (optional $ pSatisfyEq '|')

pT' :: BoolExpr -> Parser BoolExpr
pT' expr = pE'T' pF pT' (('&', And) NE.:| []) expr <* pSpaces
-- pT' = pE'T' pF pT' (optional $ pSatisfyEq '&')

pT :: Parser BoolExpr
pT = do
  pSpaces
  f <- pF
  pT' f

pExtract :: Bool -> Parser BoolExpr
pExtract neg = pSatisfyEq '(' *> pE neg <* pSatisfyEq ')'

pVal :: Parser BoolExpr
pVal = case3 <|> case4 <|> case1 <|> case2 where
  abstract12 b c = (BoolOp $ BoolNeg $ BoolVal b) <$ (pSign '~' >> pSatisfyEq c)
  case1 = abstract12 False '0'
  case2 = abstract12 True '1'
  abstract34 b c = BoolVal b <$ pSatisfyEq c
  case3 = abstract34 False '0'
  case4 = abstract34 True '1'
-- pVal = do
--     neg <- pSign '~'
--     val <- optional $ pSatisfyEq '0' <|> pSatisfyEq '1'
--     pSpaces
--     case neg of
--         Just _ -> case val of
--             Just '0' -> pure $ BoolOp $ BoolNeg $ BoolVal False
--             Just '1' -> pure $ BoolOp $ BoolNeg $ BoolVal True
--             _        -> empty
--         Nothing -> case val of
--             Just '0' -> pure $ BoolVal False
--             Just '1' -> pure $ BoolVal True
--             _        -> empty

pVar :: Parser BoolExpr
pVar = ((\x -> Var [x]) <$> case1) <|> case2 where
  case1 = mfilter isAsciiLower pChar
  case2 = (\x -> BoolOp $ BoolNeg $ Var [x]) <$> (pSign '~' >> case1)
-- pVar = do
--     neg <- pSign '~'
--     val <- optional $ mfilter (\c -> c >= 'a' && c <= 'z') pChar
--     pSpaces
--     case neg of
--         Just _ -> case val of
--             Just x  -> pure $ BoolOp $ BoolNeg $ Var [x]
--             Nothing -> empty
--         Nothing -> case val of
--             Just x  -> pure $ Var [x]
--             Nothing -> empty

pF :: Parser BoolExpr
pF = pVal <|> pVar <|> pExtract False <|> (pSatisfyEq '~' >> pSpaces *> pExtract True)

parseBoolExpr :: String -> Except ParseError BoolExpr
parseBoolExpr = runP (pE False <* pEof)

push :: String -> String
push input = case parseBoolExpr input of
    err@(Error _)   -> show err
    Success success -> show $ push' success

push' :: BoolExpr -> BoolExpr
push' = \case
  BoolOp (BoolNeg (BoolOp (BoolNeg x))) -> push' x
  BoolOp (BoolNeg (BoolVal False)) -> BoolVal True
  BoolOp (BoolNeg (BoolVal True)) -> BoolVal False
  BoolOp (BoolNeg v@(Var _)) -> BoolOp $ BoolNeg v
  BoolOp (BoolNeg (BoolOp (And x y))) ->
    BoolOp $ Or (push' $ BoolOp $ BoolNeg x) (push' $ BoolOp $ BoolNeg y)
  BoolOp (BoolNeg (BoolOp (Or x y))) ->
    BoolOp $ And (push' $ BoolOp $ BoolNeg x) (push' $ BoolOp $ BoolNeg y)
  BoolOp (And x y) -> BoolOp $ And (push' x) (push' y)
  BoolOp (Or x y) -> BoolOp $ Or (push' x) (push' y)
  v -> v

{-
In order to exercise one more time in making hand made syntax analyzers
let us do the following exercise.
Given array, for example:
1. [1, [2, 3], [4, [[5, 6]]], 7, [8]]
2. [1, [2, [3, [4, [5], 6], 7], 8], 9]
3. [1, 2, [3], [4, 5], [6, [7, 8]], [9, [10]]]
Need to calculate function on array according to the following rules:
1. If array has odd enclosure, then ordinary sum of it's elements is
calculating,
2. Otherwise an alternating sum of it's elements is calculating.
Examples:
1. [1, [2, 3], [4, [[5, 6]]], 7, [8]] -> 1 + (2 - 3) + (4 - (5 - 6)) + 7 + 8 = 20
2. [1, [2, [3, [4, [5], 6], 7], 8], 9] -> 1 + (2 - (3 + (4 - 5 + 6) + 7) + 8) + 9 = 5
Input string can contain spaces, in case of incorrect input string output is string
"Error".

To do this exercise we can use the following grammar:
1. E0 -> [E1]
2. E1 -> n, E1
3. E1 -> E0
4. E1 -> n
5. E1 -> E0, E1
-}

data MN =
      N Integer
    | M [MN]
    deriving Show

pE0 :: Parser MN
pE0 = M <$> (pSpaces >> pSatisfyEq '[' *> pE1 <* (pSatisfyEq ']' >> pSpaces))

pH1 :: Parser MN
pH1 = N . read <$> (pSpaces *> pDigits <* pSpaces)

pH2 :: Parser [MN]
pH2 = pH1 <* (pSatisfyEq ',' >> pSpaces) >>= \t -> pE1 >>= \d -> return $ t : d

pH3 :: Parser [MN]
pH3 = pSpaces *> pE0 <* pSCS >>= \t -> pE1 >>= \d -> return $ t : d where
  pSCS = pSpaces >> pSatisfyEq ',' >> pSpaces

pE1 :: Parser [MN]
pE1 = pH3 <|> arr pE0 <|> pH2 <|> arr pH1 where
  arr p = (:[]) <$> p

parseMN :: String -> Except ParseError MN
parseMN = runP $ pE0 <* pEof

solve :: String -> String
solve str = case parseMN str of
  Error _        -> "Error"
  Success (M mn) -> show $ solve' True mn
  Success (N n)  -> show n

solve' ::  Bool -> [MN] -> Integer
solve' False = foldr f 0 where
  f (M m) acc = solve' True m - acc
  f (N n) acc = n - acc
solve' True = foldr f 0 where
  f (M m) acc = solve' False m + acc
  f (N n) acc = n + acc

main :: IO ()
main = do
    print $ parseExpr "  -     (0.23+((     (3    ))*    5)/100.001) -10.23  "
    print $ parseBoolExpr "(1|~0&~a|b&d)"
    -- let Success f = parseBoolExpr "(1|~0&~a|b&d)&(~a&~b)|~(~(~a&~0|~1&~b|d))"
    -- putStrLn $ show $ runES (evalBool (Map.fromList [("a", False), ("b", False), ("d", False)]) f) []
    print $ push "(1|~0&~a|b&d)&(~a&~b)|~(~(~a&~0|~1&~b|d))"
    print $ push "     ~ (~(       ~a|~     (  b& ~d|   ~  (   0|    a)))    & a) | ((a& c))"
    print $ parseExpr "(1 * (2 - 3))"
    print $ solve "[1, [2, 3], [4, [[5, 6]]], 7, [8]]"
    print $ solve "[1, [2, [3, [4, [5], 6], 7], 8], 9]"
    print $ solve "[]"
    print $ solve "[1, 2, 3]"
    print $ solve "[[1, [2, 3]], [[4, 5, 6], 7], [8, 9, [10, 11]]]"
