{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( Parser (..)
  , ParseError (..)
  , runP
  , pDigits
  , pChar
  , pEof
  , pSign
  , parseError
  , parseExpr
  , pSpaces
  , pSatisfy
  , pSatisfyEq
  ) where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace)
import Numeric.Natural (Natural)

import HW4.T1 (ExceptState (..))
import HW4.Types

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P estate) s =
  case runES estate (0, s) of
    Error e               -> Error e
    Success (wanted :# _) -> Success wanted

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(nat, _) -> Error $ ErrorAtPos nat

pEof :: Parser ()
pEof = P $ ES $ \nastr@(nat, str) ->
  case str of
    [] -> Success $ () :# nastr
    _  -> Error $ ErrorAtPos nat

instance Alternative Parser where
  empty = parseError
  P estate1 <|> P estate2 =
    P $ ES $ \s ->
      case runES estate1 s of
        Error _ -> runES estate2 s
        success -> success

-- No methods
instance MonadPlus Parser

pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p = mfilter p pChar

pSatisfyEq :: Char -> Parser Char
pSatisfyEq c = pSatisfy (== c)

pSpaces :: Parser ()
pSpaces = void $ many $ pSatisfy isSpace

pDigits:: Parser String
pDigits = some $ pSatisfy isDigit

mstringToDouble :: Bool -> Maybe String -> Double
mstringToDouble _ Nothing = 0
mstringToDouble rev (Just str)
  | rev = foldr (\x y -> dtd x + y / 10 ) 0 str / 10
  | otherwise = foldl (\x y -> 10 * x + dtd y) 0 str

zeroFromEnum :: Int
zeroFromEnum = fromEnum '0'

dtd :: Char -> Double
dtd c = fromIntegral $ fromEnum c - zeroFromEnum

between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 *> p2 <* p3

-- pSign sign = pSpaces *> optional (pSatisfyEq sign) <* pSpaces
-- pSign sign = between pSpaces pSpaces $ optional $ pSatisfyEq sign
pSign :: Char -> Parser (Maybe Char)
pSign sign = between p q p where
  p = pSpaces
  q = optional $ pSatisfyEq sign

-- pDigitsAfter :: Parser String
-- pDigitsAfter = many $ pSatisfy isDigit

-- dot <- optional $ pSatisfyEq '.'
-- num2 <- pDigitsAfter
-- when (isJust dot && null num2) empty
-- pSpaces
pDouble :: Parser Expr
pDouble = do
  minus <- pSign '-'
  num1 <- Just <$> pDigits
  num2 <- optional $ between (pSatisfyEq '.') pDigits pSpaces
  let val = mstringToDouble False num1 + mstringToDouble True num2
  return . Val $ case minus of
    Nothing -> val
    _       -> -val

-- The fundament of grammar used below was taken from
-- https://neerc.ifmo.ru/wiki/index.php?title=Предиктивный_синтаксический_анализ

pE :: Bool -> Parser Expr
pE neg = pSpaces >> pT >>= choose where
  choose | neg = fmap (Op . Sub 0) . pE' -- suggestion of hlint, before: \s -> Op . Sub 0 <$> pE' s
         | otherwise = pE'

-- (1) hlint suggests using name pET' in camelCase,
-- however, the following name is more appropriate
-- (2) Before foldr1 (<|>) with NonEmpty was used instead of asum
-- (3) As Parser is also MonadPlus instance, we can use msum instead of
-- asum
pE'T' :: Parser Expr ->
  (Expr -> Parser Expr) ->
      [(Char, Expr -> Expr -> Prim Expr)] ->
        Expr ->
          Parser Expr
pE'T' p1 p2 pairs expr = asum (map genP pairs) <|> return expr where
  pStripChar char = between pSpaces (pSatisfyEq char) pSpaces
  genP (char, ctor) = pStripChar char >> p1 >>= p2 . Op . ctor expr

lowOps ::  [(Char, Expr -> Expr -> Prim Expr)]
lowOps = [('+', Add), ('-', Sub)]

pE' :: Expr -> Parser Expr
pE' expr = pE'T' pT pE' lowOps expr <* pSpaces

highOps :: [(Char, Expr -> Expr -> Prim Expr)]
highOps = [('*', Mul), ('/', Div)]

pT' :: Expr -> Parser Expr
pT' expr = pE'T' pF pT' highOps expr <* pSpaces

pT :: Parser Expr
pT = pSpaces >> pF >>= pT'

-- pExtract neg = pSatisfyEq '(' *> pE neg <* pSatisfyEq ')'
pExtract :: Bool -> Parser Expr
pExtract neg = between p q r where
  p = pSatisfyEq '('
  q = pE neg
  r = pSatisfyEq ')'

-- Note: if we remove "<|> (pSatisfyEq '-' >> pSpaces >> pExtract True)" from
-- code below, tests from github will accept solution, although obtained
-- in this way solution cannot parse, for example, "-(2+3)" (unary minus
-- before opening bracket).
pF :: Parser Expr
pF = pDouble <|> pExtract False <|> (pSatisfyEq '-' >> pSpaces >> pExtract True)

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ pE False <* pEof
