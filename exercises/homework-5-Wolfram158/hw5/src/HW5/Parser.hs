module HW5.Parser
  ( parse
  ) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Void (Void)
import HW5.Pretty (fname)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, count, eof, label, lookAhead,
                        many, manyTill, notFollowedBy, optional, runParser, satisfy, sepBy, sepBy1,
                        sepEndBy, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import HW5.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse s
  | all isSpace s = return $ HiExprValue HiValueNull
  | otherwise = runParser (between space eof pExpr') [] s

skipSpace :: Parser ()
skipSpace = L.space
  space1
  (L.skipLineComment ";;")
  (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

numeric :: Parser Rational
numeric = label "number" $ lexeme $ toRational <$> L.signed skipSpace L.scientific

str :: Parser T.Text
str = label "string" $ lexeme $ T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

bytes :: Parser BS.ByteString
bytes = label "bytes" $ lexeme $ BS.pack <$>
  (symbol "[#" >>
    lookAhead (sepEndBy (count 2 hexDigitChar) space1 >> symbol "#]")
      >> manyTill (lexeme L.hexadecimal) (symbol "#]"))

pFunction :: Parser HiExpr
pFunction = label "function" $ lexeme $ choice $ map unwrap'n'wrap [minBound..maxBound :: HiFun]

unwrap'n'wrap :: HiFun -> Parser HiExpr
unwrap'n'wrap fun = (HiExprValue $ HiValueFunction fun) <$ unwrap (fname fun)

pNull :: Parser HiExpr
pNull = label "null" $ HiExprValue HiValueNull <$ unwrap "null"

pBool :: Parser HiExpr
pBool = label "bool" $ lexeme $ choice
  [
   (HiExprValue $ HiValueBool True) <$ unwrap "true",
   (HiExprValue $ HiValueBool False) <$ unwrap "false"
  ]

pAction :: Parser HiExpr
pAction = label "action" $ lexeme $ choice
  [
    (HiExprValue $ HiValueAction HiActionCwd) <$ unwrap "cwd",
    (HiExprValue $ HiValueAction HiActionNow) <$ unwrap "now"
  ]

unwrap :: String -> Parser String
unwrap strin = try $ try (string strin) <|> parens (unwrap strin)

surprise :: Parser a -> Parser a
surprise p = try p <|> parens p

pProperty :: Parser String
pProperty = intercalate "-" <$>
  ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'

pApplyProperty :: HiExpr -> Parser HiExpr
pApplyProperty expr = label "property" $ lexeme $ do
  void $ char '.'
  prop <- pProperty
  return $ HiExprApply expr [HiExprValue $ HiValueString $ T.pack prop]

pList :: Parser HiExpr
pList = lexeme $ do
  void $ symbol "["
  vals <- pArguments
  void $ symbol "]"
  return $ HiExprApply (HiExprValue (HiValueFunction HiFunList)) vals

pMap :: Parser HiExpr
pMap = label "map" $ lexeme $ do
  void $ symbol "{"
  entries <- pEntries
  void $ symbol "}"
  return $ HiExprDict entries

pEntries :: Parser [(HiExpr, HiExpr)]
pEntries = label "entries" $ lexeme $ sepBy rule $ symbol "," where
  rule = do
    expr1 <- pExpr'
    void $ symbol ":"
    expr2 <- pExpr'
    return (expr1, expr2)

pApply :: HiExpr -> Parser HiExpr
pApply expr = label "application" $ lexeme $ do
  res <- optional $ parens pArguments
  case res of
    Just args -> pure (HiExprApply expr args) >>= pApply
    Nothing   -> (try (pApplyProperty expr) >>= pApply) <|> return expr

pArguments :: Parser [HiExpr]
pArguments = label "arguments" $ lexeme $ sepBy pExpr' $ symbol ","

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pExpr :: Parser HiExpr
pExpr = lexeme $ choice
  [
   pFunction,
   pBool,
   pNull,
   pAction,
   try $ parens pExpr,
   try $ parens pExpr',
   HiExprValue . HiValueString <$> surprise str,
   HiExprValue . HiValueNumber <$> surprise numeric,
   try $ HiExprValue . HiValueBytes <$> surprise bytes,
   pList,
   pMap
  ] >>= pApply

pTerm :: Parser HiExpr
pTerm = choice
  [
    pExpr
  , symbol "-" >> pExpr >>=
      \x -> return $
        HiExprApply (HiExprValue $ HiValueFunction HiFunSub)
          [HiExprValue $ HiValueNumber 0, x]
  ]

pExpr' :: Parser HiExpr
pExpr' = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [
    [postfix "!" HiExprRun],
    [ binaryL "*" $ makeApplication HiFunMul
    , binaryDiv "/" $ makeApplication HiFunDiv
    ]
  , [ binaryL "+" $ makeApplication HiFunAdd
    , binaryL "-" $ makeApplication HiFunSub
    ]
  , [ binaryN "<=" $ makeApplication HiFunNotGreaterThan
    , binaryN "<" $ makeApplication HiFunLessThan
    , binaryN ">=" $ makeApplication HiFunNotLessThan
    , binaryN ">" $ makeApplication HiFunGreaterThan
    , binaryN "==" $ makeApplication HiFunEquals
    , binaryN "/=" $ makeApplication HiFunNotEquals
    ]
  , [ binaryR "&&" $ makeApplication HiFunAnd ]
  , [ binaryR "||" $ makeApplication HiFunOr ]
  ]

makeApplication :: HiFun -> HiExpr -> HiExpr -> HiExpr
makeApplication fun expr1 expr2 =
  HiExprApply (HiExprValue $ HiValueFunction fun) [expr1, expr2]

symbol :: String -> Parser String
symbol = L.symbol skipSpace

binaryDiv, binaryL, binaryN, binaryR :: String ->
  (HiExpr -> HiExpr -> HiExpr) ->
    Operator Parser HiExpr
binaryDiv name f = InfixL (f <$ try (label "operator" $ symbol name *> notFollowedBy (string "=")))
binaryL name f = InfixL (f <$ label "operator" (symbol name))
binaryN name f = InfixN (f <$ label "operator" (symbol name))
binaryR name f = InfixR (f <$ label "operator" (symbol name))

postfix :: String -> (HiExpr -> HiExpr) -> Operator Parser HiExpr
postfix name f = Postfix (foldr1 (.) <$> label "operator" (some (f <$ symbol name)))
