module HW5.Pretty
  ( prettyValue
  , prettyError
  , fname
  )
  where

import qualified Data.ByteString as BS
import Data.Char (isUpper, toLower)
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Ratio (denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited)
import qualified Data.Sequence as S
import Numeric (showHex)
import Prettyprinter (Doc, annotate, colon, comma, hsep, lbrace, lbracket, pretty, punctuate,
                      rbrace, rbracket, rparen, slash, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Blue, Cyan, Green, Magenta, Red, Yellow),
                                      bold, color)

import HW5.Base (HiAction (..), HiError (..), HiFun (..), HiValue (..))

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber r) = do
  let num = numerator r
  case denominator r of
    1 -> annotate bold $ pretty num
    denom -> case fromRationalRepetendUnlimited r of
      (x, Nothing) -> annotate bold $ viaShow x
      _ -> do
        let (q, rest) = quotRem num denom
        if q == 0
          then annotate bold $ pretty num <> slash <> pretty denom
          else
            case signum rest of
              -1 ->  annotate bold $
                pretty q <+> pretty "-" <+> pretty (abs rest) <> slash <> pretty denom
              _  -> annotate bold $
                pretty q <+> pretty "+" <+> pretty rest <> slash <> pretty denom
prettyValue (HiValueAction (HiActionRead path)) = pretty "read(" <> pretty path <> rparen
prettyValue (HiValueAction (HiActionWrite path bs)) =
  pretty "write(" <> pretty path <> comma <+> prettyValue (HiValueBytes bs) <> rparen
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueAction (HiActionChDir path)) =
  pretty "cd(" <> pretty path <> rparen
prettyValue (HiValueAction (HiActionMkDir path)) =
  pretty "mkdir(" <> pretty path <> rparen
prettyValue (HiValueAction (HiActionRand l r)) =
  pretty "rand(" <> pretty l <> comma <+> pretty r <> rparen
prettyValue (HiValueAction (HiActionEcho text)) =
  pretty "echo(" <> viaShow text <> rparen
prettyValue (HiValueBool boo) = annotate (color Green) $ pretty $ map toLower $ show boo
prettyValue HiValueNull = annotate (color Magenta) $ pretty "null"
prettyValue (HiValueList list)
  | S.null list = pretty "[]"
  | otherwise = lbracket <+> content <+> rbracket where
      content = hsep $ punctuate comma $ map prettyValue $ toList list
prettyValue (HiValueString text) = annotate (color Cyan) $ viaShow text
prettyValue (HiValueBytes bytes)
  | BS.null bytes = pretty "[# #]"
  | otherwise = pretty "[#" <+> content <+> pretty "#]" where
      content = hsep $ map (pretty . transform) $ BS.unpack bytes
      transform t =
        case showHex t [] of
        u@[_] -> '0' : u
        els   -> els
prettyValue (HiValueTime time) = annotate (color Blue) $ viaShow time
prettyValue (HiValueDict dict)
  | Map.null dict = lbrace <> rbrace
  | otherwise = lbrace <+> content <+> rbrace where
      mapper (key, value) = prettyValue key <+> colon <+> prettyValue value
      content = hsep $ punctuate comma $ map mapper $ Map.toList dict
prettyValue (HiValueFunction fun) = annotate (color Yellow) $ pretty $ fname fun

fname :: HiFun -> String
fname HiFunChDir = "cd"
fname HiFunMkDir = "mkdir"
fname fun =
  map toLower $ intercalate "-" $ drop 1 $
    foldr phi [] (drop 5 $ show fun) where
      phi x [] | isUpper x = []:[[x]]
               | otherwise = [[x]]
      phi x (y:ys) | isUpper x = []:(x:y):ys
                   | otherwise = (x:y):ys

prettyError :: HiError -> Doc AnsiStyle
prettyError = annotate (color Red) . viaShow
