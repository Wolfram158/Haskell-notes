{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module HW5.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib (bestCompression, compressLevel, compressWith, decompress,
                               defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad (liftM2, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime, secondsToDiffTime)
import Data.Tuple (swap)
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..))
import Prelude hiding (seq)
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalT

dotCase :: (Semigroup a, HiMonad m) => a -> Rational -> ExceptT HiError m a
dotCase sgroup rat =
  do
    when (numerator rat <= 0 || denominator rat /= 1) $
      throwError HiErrorInvalidArgument
    return $ stimes (numerator rat) sgroup

evalAdd, evalDot, evalSub, evalDiv, evalEq, evalNotEq,
  evalLessThan, evalNotLessThan, evalGreaterThan, evalNotGreaterThan,
    evalRange, evalFold, evalWrite, evalRand ::
      HiMonad m => (HiValue, HiValue) -> ExceptT HiError m HiValue
evalAdd pair = case pair of
    (HiValueTime time1, HiValueNumber n2) ->
      do
        let n2d = secondsToDiffTime $ numerator n2
        return . HiValueTime $ addUTCTime (fromRational $ toRational n2d) time1
    p@(HiValueNumber{}, HiValueTime{}) -> evalAdd $ swap p
    (HiValueNumber n1, HiValueNumber n2) -> return $ HiValueNumber $ n1 + n2
    (HiValueString text1, HiValueString text2) -> return $ HiValueString $ text1 <> text2
    (HiValueList seq1, HiValueList seq2) -> return $ HiValueList $ seq1 <> seq2
    (HiValueBytes bs1, HiValueBytes bs2) -> return $ HiValueBytes $ bs1 <> bs2
    _ -> throwError HiErrorInvalidArgument

evalDot pair = case pair of
    (HiValueNumber n1, HiValueNumber n2)    -> return $ HiValueNumber $ n1 * n2
    (HiValueString text1, HiValueNumber n2) -> HiValueString <$> dotCase text1 n2
    (HiValueList seq1, HiValueNumber n2)    -> HiValueList <$> dotCase seq1 n2
    (HiValueBytes bs1, HiValueNumber n2)    -> HiValueBytes <$> dotCase bs1 n2
    p1@(HiValueNumber{}, HiValueString{})   -> evalDot $ swap p1
    p2@(HiValueNumber{}, HiValueList{})     -> evalDot $ swap p2
    p3@(HiValueNumber{}, HiValueBytes{})    -> evalDot $ swap p3
    _                                       -> throwError HiErrorInvalidArgument

evalSub = \case
    (HiValueTime time1, HiValueTime time2) ->
      return $ HiValueNumber $ toRational $ diffUTCTime time1 time2
    (HiValueNumber n1, HiValueNumber n2) -> return $ HiValueNumber $ n1 - n2
    _ -> throwError HiErrorInvalidArgument

evalDiv = \case
    (HiValueNumber n1, HiValueNumber n2) ->
      do
        when (numerator n2 == 0) $ throwError HiErrorDivideByZero
        return $ HiValueNumber $ n1 / n2
    (HiValueString text1, HiValueString text2) ->
      return $ HiValueString $ text1 <> T.pack "/" <> text2
    _ -> throwError HiErrorInvalidArgument

evalEq = evalCompare (==)
evalNotEq = evalCompare (/=)
evalLessThan = evalCompare (<)
evalGreaterThan = evalCompare (>)
evalNotLessThan = evalCompare (>=)
evalNotGreaterThan = evalCompare (<=)

evalRange = \case
    (HiValueNumber n, HiValueNumber m) ->
      return $ HiValueList $ HiValueNumber <$> S.fromList [n..m]
    _ -> throwError HiErrorInvalidArgument

{-
if S.null seq
        then return HiValueNull
        else evalT $ foldl1 (\x y -> HiExprApply (HiExprValue fun) [x, y]) $
          map HiExprValue $ toList seq
-}
evalFold = \case
    (fun, HiValueList (h S.:<| rest)) ->
      evalT $
        foldl
          (\x y -> HiExprApply (HiExprValue fun) [x, y])
            (HiExprValue h) $
              map HiExprValue $ toList rest
    (_, HiValueList _) -> return HiValueNull
    _ -> throwError HiErrorInvalidArgument

evalWrite = \case
    (HiValueString text1, HiValueString text2) ->
      return $
        HiValueAction $
          HiActionWrite (T.unpack text1) $
            encodeUtf8 text2
    _ -> throwError HiErrorInvalidArgument

evalRand = \case
    (HiValueNumber n1, HiValueNumber n2) ->
      return $
          HiValueAction $
            HiActionRand
              (fromIntegral $ numerator n1)
                (fromIntegral $ numerator n2)
    _ -> throwError HiErrorInvalidArgument

evalCompare :: HiMonad m => (a -> a -> Bool) -> (a, a) -> ExceptT HiError m HiValue
evalCompare p (x, y) = return $ HiValueBool $ p x y

evalIf :: HiMonad m => (HiValue, HiExpr, HiExpr) -> ExceptT HiError m HiValue
evalIf = \case
    (HiValueBool True, expr, _) -> evalT expr
    (HiValueBool False, _, expr) -> evalT expr
    _ -> throwError HiErrorInvalidArgument

evalAnd :: HiMonad m => (HiValue, HiExpr) -> ExceptT HiError m HiValue
evalAnd = \case
    (false@(HiValueBool False), _) -> return false
    (nil@HiValueNull, _) -> return nil
    (_, val) -> evalT val

evalOr :: HiMonad m => (HiValue, HiExpr) -> ExceptT HiError m HiValue
evalOr = \case
    (HiValueBool False, val) -> evalT val
    (HiValueNull, val) -> evalT val
    (val, _) -> return val

evalNot, evalLength, evalToUpper, evalToLower, evalTrim, evalEncodeUtf8,
  evalDecodeUtf8, evalZip, evalUnzip, evalSerialise, evalDeserialise,
    evalPackBytes, evalUnpackBytes, evalRead, evalParseTime, evalEcho,
      evalChDir, evalMkDir, evalKeys, evalCount, evalValues, evalInvert,
        evalReverse ::
          HiMonad m => HiValue -> ExceptT HiError m HiValue
evalNot = \case
    HiValueBool boo -> return $ HiValueBool $ not boo
    _ -> throwError HiErrorInvalidArgument

evalLength = \case
    HiValueString text -> lengthCase $ T.length text
    HiValueList seq -> lengthCase $ S.length seq
    HiValueBytes bs -> lengthCase $ BS.length bs
    _ -> throwError HiErrorInvalidArgument

evalReverse = \case
    HiValueString text -> reverseCase text T.reverse HiValueString
    HiValueList seq -> reverseCase seq S.reverse HiValueList
    HiValueBytes bs -> reverseCase bs BS.reverse HiValueBytes
    _ -> throwError HiErrorInvalidArgument

evalToUpper = \case
    HiValueString text -> return $ HiValueString $ T.toUpper text
    _ -> throwError HiErrorInvalidArgument

evalToLower = \case
    HiValueString text -> return $ HiValueString $ T.toLower text
    _ -> throwError HiErrorInvalidArgument

evalTrim = \case
    HiValueString text -> return $ HiValueString $ T.strip text
    _ -> throwError HiErrorInvalidArgument

evalEncodeUtf8 = \case
    HiValueString text -> return $ HiValueBytes $ encodeUtf8 text
    _ -> throwError HiErrorInvalidArgument

evalDecodeUtf8 = \case
    HiValueBytes bs ->
      case decodeUtf8' bs of
        Left _     -> return HiValueNull
        Right text -> return $ HiValueString text
    _ -> throwError HiErrorInvalidArgument

evalZip = \case
    HiValueBytes bs ->
      return $ HiValueBytes $ BS.toStrict $
        compressWith (defaultCompressParams { compressLevel = bestCompression }) $
          BS.fromStrict bs
    _ -> throwError HiErrorInvalidArgument

evalUnzip = \case
    HiValueBytes bs ->
      return $ HiValueBytes $ BS.toStrict $
        decompress $
          BS.fromStrict bs
    _ -> throwError HiErrorInvalidArgument

evalSerialise val = return $ HiValueBytes $ BS.toStrict $ serialise val

evalDeserialise = \case
    HiValueBytes bs ->
      case deserialiseOrFail $ BS.fromStrict bs of
        Left _  -> throwError HiErrorInvalidArgument
        Right r -> return r
    _ -> throwError HiErrorInvalidArgument

evalPackBytes = \case
    HiValueList seq -> HiValueBytes . BS.pack <$> (check . toList) seq
    _ -> throwError HiErrorInvalidArgument

evalUnpackBytes = \case
  HiValueBytes bs ->
    return $ HiValueList $ S.fromList $
      map (HiValueNumber . toRational) $ BS.unpack bs
  _ -> throwError HiErrorInvalidArgument

evalRead = \case
    HiValueString text -> return $ HiValueAction $ HiActionRead $ T.unpack text
    _ -> throwError HiErrorInvalidArgument

evalParseTime = \case
    HiValueString text ->
      return $ maybe HiValueNull HiValueTime $ readMaybe (T.unpack text)
    _ -> throwError HiErrorInvalidArgument

evalEcho = \case
    HiValueString text -> return $ HiValueAction $ HiActionEcho text
    _ -> throwError HiErrorInvalidArgument

evalChDir = \case
    HiValueString text -> return $ HiValueAction $ HiActionChDir $ T.unpack text
    _                  -> throwError HiErrorInvalidArgument

evalMkDir = \case
    HiValueString text -> return $ HiValueAction $ HiActionMkDir $ T.unpack text
    _                  -> throwError HiErrorInvalidArgument

evalKeys = \case
    HiValueDict dict ->
      return $ HiValueList $ S.fromList $ Map.keys dict
    _ -> throwError HiErrorInvalidArgument

evalValues = \case
    HiValueDict dict ->
      return $ HiValueList $ S.fromList $ Map.elems dict
    _ -> throwError HiErrorInvalidArgument

evalCount = \case
    HiValueString text ->
      return $ HiValueDict $ Map.map HiValueNumber $
        Map.fromListWith (+) $ map (\p -> (HiValueString $ T.pack [p], 1)) $
          T.unpack text
    HiValueList list ->
      return $ HiValueDict $ Map.map HiValueNumber $
        Map.fromListWith (+) $ map (, 1) $ toList list
    bs@(HiValueBytes _) ->
      do
        nums <- evalT $
          HiExprApply (HiExprValue $ HiValueFunction HiFunUnpackBytes) [HiExprValue bs]
        case nums of
          HiValueList list ->
            return $ HiValueDict $ Map.map HiValueNumber $
              Map.fromListWith (+) $ map (, 1) $ toList list
          _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

evalInvert = \case
    HiValueDict dict ->
      return $ HiValueDict $ Map.map (HiValueList . S.fromList) $
        Map.fromListWith (<>) pairs where
          pairs = [(v, [k]) | (k, v) <- Map.toList dict]
    _ -> throwError HiErrorInvalidArgument

evalFunList :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
evalFunList exprs = HiValueList . S.fromList <$> traverse evalT exprs

lengthCase :: HiMonad m => Int -> ExceptT HiError m HiValue
lengthCase = return . HiValueNumber . fromIntegral

reverseCase :: HiMonad m => a -> (a -> a) -> (a -> HiValue) -> ExceptT HiError m HiValue
reverseCase content rev ctor = return $ ctor $ rev content

binaries :: HiMonad m => Map.Map HiFun ((HiValue, HiValue) -> ExceptT HiError m HiValue)
binaries = Map.fromList [ (HiFunAdd, evalAdd), (HiFunMul, evalDot), (HiFunSub, evalSub)
                        , (HiFunDiv, evalDiv), (HiFunNotLessThan, evalNotLessThan)
                        , (HiFunLessThan, evalLessThan)
                        , (HiFunGreaterThan, evalGreaterThan), (HiFunNotEquals, evalNotEq)
                        , (HiFunNotGreaterThan, evalNotGreaterThan), (HiFunRange, evalRange)
                        , (HiFunRand, evalRand), (HiFunFold, evalFold)
                        , (HiFunWrite, evalWrite), (HiFunEquals, evalEq) ]

evalSlice :: HiMonad m => a -> (Int -> a -> (a, a)) -> Int ->
  (a -> HiValue) -> (HiValue, HiValue) -> ExceptT HiError m HiValue
evalSlice content split len ctor = \case
    (HiValueNull, val) -> evalSlice content split len ctor (HiValueNumber 0, val)
    (num@(HiValueNumber n), HiValueNull) ->
      do
        when (denominator n /= 1) $ throwError HiErrorInvalidArgument
        evalSlice content split len ctor (num, HiValueNumber $ fromIntegral len)
    (HiValueNumber n, HiValueNumber m) ->
      do
        when (denominator n /= 1) $ throwError HiErrorInvalidArgument
        (left, right) <- reduceDuplication n m len
        let (_, t2) = split left content
        let (t1, _) = split (right - left) t2
        return $ ctor t1
    _ -> throwError HiErrorInvalidArgument

evalIndex :: HiMonad m => Int -> (Int -> HiValue) -> HiValue -> ExceptT HiError m HiValue
evalIndex len get = \case
  HiValueNumber n ->
    do
      let ind = numerator n
      when (denominator n /= 1) $ throwError HiErrorInvalidArgument
      if ind < 0
        then return HiValueNull
      else
        if len > fromIntegral ind
          then return $ get $ fromIntegral ind
        else return HiValueNull
  _ -> throwError HiErrorInvalidArgument

unaries :: HiMonad m => Map.Map HiFun (HiValue -> ExceptT HiError m HiValue)
unaries = Map.fromList [ (HiFunLength, evalLength), (HiFunToUpper, evalToUpper)
                       , (HiFunToLower, evalToLower), (HiFunReverse, evalReverse)
                       , (HiFunTrim, evalTrim), (HiFunPackBytes, evalPackBytes)
                       , (HiFunUnpackBytes, evalUnpackBytes), (HiFunEncodeUtf8, evalEncodeUtf8)
                       , (HiFunDecodeUtf8, evalDecodeUtf8), (HiFunZip, evalZip)
                       , (HiFunUnzip, evalUnzip), (HiFunSerialise, evalSerialise)
                       , (HiFunDeserialise, evalDeserialise), (HiFunRead, evalRead)
                       , (HiFunMkDir, evalMkDir), (HiFunChDir, evalChDir)
                       , (HiFunParseTime, evalParseTime), (HiFunEcho, evalEcho)
                       , (HiFunCount, evalCount), (HiFunKeys, evalKeys)
                       , (HiFunValues, evalValues), (HiFunInvert, evalInvert)
                       , (HiFunNot, evalNot) ]

evalT :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalT (HiExprApply app args) =
  do
    appeval <- evalT app
    case appeval of
      HiValueFunction HiFunList -> evalFunList args
      HiValueBool _ -> throwError HiErrorInvalidFunction
      HiValueNumber _ -> throwError HiErrorInvalidFunction
      HiValueNull -> throwError HiErrorInvalidFunction
      _ ->
        case args of
          [] ->
            case appeval of
              HiValueFunction _ -> throwError HiErrorArityMismatch
              _                 -> throwError HiErrorInvalidFunction
          [x] -> evalT1 appeval x
          [x, y] -> evalT2 appeval x y
          [x, y, z] -> evalT3 appeval x y z
          _ -> throwError HiErrorArityMismatch

evalT (HiExprValue val) = return val

evalT (HiExprRun r) =
  do
    reval <- evalT r
    case reval of
      HiValueAction act -> lift $ runAction act
      _                 -> throwError HiErrorInvalidArgument

evalT (HiExprDict pairs) =
  HiValueDict . Map.fromList <$> mapM mapper pairs where
    mapper (x, y) = liftM2 (,) (evalT x) (evalT y)
  -- do
  --   let mapper (x, y) = liftM2 (,) (evalT x) (evalT y)
  --   vpairs <- mapM mapper pairs
  --   return $ HiValueDict $ Map.fromList vpairs

evalT2 :: HiMonad m => HiValue -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalT2 (HiValueFunction HiFunAnd) x y = evalT x >>= evalAnd . (, y)
evalT2 (HiValueFunction HiFunOr) x y = evalT x >>= evalOr . (, y)
evalT2 (HiValueFunction fun) x y =
  maybe
    (throwError HiErrorArityMismatch)
      (\eval' -> liftM2 (,) (evalT x) (evalT y) >>= eval') $
        Map.lookup fun binaries
  -- case Map.lookup fun binaries of
  --   Just eval' -> liftM2 (,) (evalT x) (evalT y) >>= eval'
  --   Nothing    -> throwError HiErrorArityMismatch
evalT2 appeval x y =
  do
    xeval <- evalT x
    yeval <- evalT y
    case appeval of
      HiValueString text ->
        evalSlice text T.splitAt (T.length text) HiValueString (xeval, yeval)
      HiValueList seq ->
        evalSlice seq S.splitAt (S.length seq) HiValueList (xeval, yeval)
      HiValueBytes bs ->
        evalSlice bs BS.splitAt (BS.length bs) HiValueBytes (xeval, yeval)
      _ -> throwError HiErrorInvalidFunction

evalT1 :: HiMonad m => HiValue -> HiExpr -> ExceptT HiError m HiValue
evalT1 (HiValueFunction fun) x =
  maybe
    (throwError HiErrorArityMismatch)
      (\eval' -> evalT x >>= eval') $
        Map.lookup fun unaries
  -- case Map.lookup fun unaries of
  --   Just eval' -> evalT x >>= eval'
  --   Nothing    -> throwError HiErrorArityMismatch
evalT1 appeval x =
  do
    xeval <- evalT x
    case appeval of
      HiValueString text ->
        evalIndex (T.length text) (HiValueString . T.singleton . T.index text) xeval
      HiValueList seq ->
        evalIndex (S.length seq) (S.index seq) xeval
      HiValueBytes bs ->
        evalIndex (BS.length bs) (HiValueNumber . toRational . BS.index bs) xeval
      HiValueDict dict ->
        return $ fromMaybe HiValueNull $ Map.lookup xeval dict
        -- case Map.lookup xeval dict of
        --   Just val -> return val
        --   Nothing  -> return HiValueNull
      _ -> throwError HiErrorInvalidFunction

evalT3 :: HiMonad m => HiValue -> HiExpr -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
evalT3 appeval x y z =
  case appeval of
    HiValueFunction HiFunIf -> evalT x >>= evalIf . (, y, z)
    HiValueFunction _       -> throwError HiErrorArityMismatch
    _                       -> throwError HiErrorInvalidFunction

check :: HiMonad m => [HiValue] -> ExceptT HiError m [Word8]
check = mapM mapper where
  mapper (HiValueNumber n) =
    do
      when (n < 0 || n > 255) $ throwError HiErrorInvalidArgument
      return $ fromIntegral $ numerator n
  mapper _ = throwError HiErrorInvalidArgument

-- This name of function is from the previous version of Evaluator (which is commented below)
reduceDuplication :: HiMonad m => Rational -> Rational -> Int -> ExceptT HiError m (Int, Int)
reduceDuplication n m len =
  do
    let nn = numerator n
    let left = if nn < 0 then len + fromIntegral nn else fromIntegral nn
    let mm = numerator m
    let right = if mm < 0 then len + fromIntegral mm else fromIntegral mm
    when (denominator m /= 1) $ throwError HiErrorInvalidArgument
    return (left, right)

-- evalT2A :: HiMonad m =>
--   HiFun -> HiValue -> HiValue ->
--     (Rational -> Rational -> Rational) ->
--       ExceptT HiError m HiValue
-- evalT2A fun xeval yeval op =
--   do
--     when (fun == HiFunDiv && yeval == HiValueNumber 0) $
--       throwError HiErrorDivideByZero
--     case xeval of
--       HiValueNumber m -> case yeval of
--         HiValueNumber n -> return . HiValueNumber $ op m n
--         _               -> throwError HiErrorInvalidArgument
--       _ -> throwError HiErrorInvalidArgument

-- evalT2B :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
-- evalT2B (HiExprApply (HiExprValue (HiValueFunction fun)) [x, y]) =
--   do
--     xeval <- evalT' x
--     case fun of
--       HiFunAnd -> case xeval of
--         HiValueBool False -> return $ HiValueBool False
--         HiValueNull       -> return HiValueNull
--         _                 -> evalT' y
--       HiFunOr -> case xeval of
--         HiValueBool False -> evalT' y
--         HiValueNull       -> evalT' y
--         true              -> return true
--       _ -> throwError HiErrorInvalidFunction
-- evalT2B (HiExprApply (HiExprValue (HiValueFunction _)) _) =
--   throwError HiErrorArityMismatch
-- evalT2B _ = throwError HiErrorInvalidFunction

-- evalTEq :: HiMonad m => HiExpr -> (Bool -> Bool) -> ExceptT HiError m HiValue
-- evalTEq (HiExprApply (HiExprValue (HiValueFunction _)) [x, y]) op =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     return . HiValueBool $ op $ xeval == yeval
-- evalTEq (HiExprApply (HiExprValue (HiValueFunction _)) _) _ =
--   throwError HiErrorArityMismatch
-- evalTEq _  _ = throwError HiErrorInvalidFunction

-- evalTIf :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
-- evalTIf (HiExprApply (HiExprValue (HiValueFunction _)) [x, y, z]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueBool False -> evalT' z
--       HiValueBool True  -> evalT' y
--       _                 -> throwError HiErrorInvalidArgument
-- evalTIf (HiExprApply app@(HiExprApply (HiExprValue (HiValueFunction HiFunIf)) _) args) =
--   do
--     aeval <- evalT' app
--     evalT' $ HiExprApply (HiExprValue aeval) args
-- evalTIf (HiExprApply (HiExprValue (HiValueFunction _)) _) =
--   throwError HiErrorArityMismatch
-- evalTIf _  = throwError HiErrorInvalidFunction

-- evalTNot :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
-- evalTNot (HiExprApply (HiExprValue (HiValueFunction _)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueBool b -> return . HiValueBool $ not b
--       _             -> throwError HiErrorInvalidArgument
-- evalTNot (HiExprApply (HiExprValue (HiValueFunction _)) _) =
--   throwError HiErrorArityMismatch
-- evalTNot _ = throwError HiErrorInvalidFunction

-- evalTLess :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
-- evalTLess (HiExprApply (HiExprValue (HiValueFunction _)) [x, y]) =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     return . HiValueBool $ xeval < yeval
-- evalTLess (HiExprApply (HiExprValue (HiValueFunction _)) _) =
--   throwError HiErrorArityMismatch
-- evalTLess _ = throwError HiErrorInvalidFunction

-- evalTSum :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
-- evalTSum [x, y] =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     case xeval of
--       HiValueString text1 -> case yeval of
--         HiValueString text2 ->
--           return $ HiValueString $ text1 <> text2
--         _ -> throwError HiErrorInvalidArgument
--       HiValueList list1 -> case yeval of
--         HiValueList list2 ->
--           return $ HiValueList $ list1 <> list2
--         _ -> throwError HiErrorInvalidArgument
--       HiValueBytes bs1 -> case yeval of
--         HiValueBytes bs2 ->
--           return $ HiValueBytes $ bs1 <> bs2
--         _ -> throwError HiErrorInvalidArgument
--       HiValueTime t1 -> case yeval of
--         HiValueNumber n2 ->
--           do
--             let n2d = secondsToDiffTime $ numerator n2
--             return . HiValueTime $ addUTCTime (fromRational $ toRational n2d) t1
--         _ -> throwError HiErrorInvalidArgument
--       HiValueNumber n1 -> case yeval of
--         HiValueTime t2 ->
--           do
--             let n1d = secondsToDiffTime $ numerator n1
--             return . HiValueTime $ addUTCTime (fromRational $ toRational n1d) t2
--         _ -> evalT2A HiFunAdd xeval yeval (+)
--       _ -> evalT2A HiFunAdd xeval yeval (+)
-- evalTSum _ = throwError HiErrorArityMismatch

-- evalTProduct :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
-- evalTProduct [x, y] =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     case xeval of
--       HiValueString text1 -> case yeval of
--         HiValueNumber n2 ->
--           do
--             when (denominator n2 /= 1) $ throwError HiErrorInvalidArgument
--             return $ HiValueString $ stimes (numerator n2) text1
--         _ -> throwError HiErrorInvalidArgument
--       HiValueList list1 -> case yeval of
--         HiValueNumber n2 ->
--           do
--             when (denominator n2 /= 1) $ throwError HiErrorInvalidArgument
--             return $ HiValueList $ stimes (numerator n2) list1
--         _ -> throwError HiErrorInvalidArgument
--       HiValueBytes bs -> case yeval of
--         HiValueNumber n2 ->
--           do
--             when (denominator n2 /= 1) $ throwError HiErrorInvalidArgument
--             return $ HiValueBytes $ stimes (numerator n2) bs
--         _ -> throwError HiErrorInvalidArgument
--       HiValueNumber n1 -> case yeval of
--         HiValueString text2 ->
--           do
--             when (denominator n1 /= 1) $ throwError HiErrorInvalidArgument
--             return $ HiValueString $ stimes (numerator n1) text2
--         HiValueList list2 ->
--           do
--             when (denominator n1 /= 1) $ throwError HiErrorInvalidArgument
--             return $ HiValueList $ stimes (numerator n1) list2
--         HiValueBytes bs ->
--           do
--             when (denominator n1 /= 1) $ throwError HiErrorInvalidArgument
--             return $ HiValueBytes $ stimes (numerator n1) bs
--         _ -> evalT2A HiFunMul xeval yeval (*)
--       _ -> evalT2A HiFunMul xeval yeval (*)
-- evalTProduct _ = throwError HiErrorArityMismatch

-- evalTDiv :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
-- evalTDiv [x, y] =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     case xeval of
--       HiValueString text1 -> case yeval of
--         HiValueString text2 ->
--           return $ HiValueString $ text1 <> T.pack "/" <> text2
--         _ -> throwError HiErrorInvalidArgument
--       _ -> evalT2A HiFunDiv xeval yeval (/)
-- evalTDiv _ = throwError HiErrorArityMismatch

-- evalTSub :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
-- evalTSub [x, y] =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     case xeval of
--       HiValueTime t1 -> case yeval of
--         HiValueTime t2 -> return $ HiValueNumber $ toRational $ diffUTCTime t1 t2
--         _              -> throwError HiErrorInvalidArgument
--       _ -> evalT2A HiFunSub xeval yeval (-)
-- evalTSub _ = throwError HiErrorArityMismatch

-- evalT' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) args) = evalTSum args
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunSub)) args) = evalTSub args
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunMul)) args) = evalTProduct args
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunDiv)) args) = evalTDiv args
-- evalT' expr@(HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) _) = evalT2B expr
-- evalT' expr@(HiExprApply (HiExprValue (HiValueFunction HiFunOr)) _) = evalT2B expr
-- evalT' expr@(HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) _) = evalTEq expr id
-- evalT' expr@(HiExprApply (HiExprValue (HiValueFunction HiFunNotEquals)) _) = evalTEq expr not
-- evalT' expr@(HiExprApply (HiExprValue (HiValueFunction HiFunIf)) _) = evalTIf expr
-- evalT' expr@(HiExprApply (HiExprApply (HiExprValue (HiValueFunction HiFunIf)) _) _) =
--   evalTIf expr
-- evalT' expr@(HiExprApply (HiExprValue (HiValueFunction HiFunNot)) _) = evalTNot expr
-- evalT' expr@(HiExprApply (HiExprValue (HiValueFunction HiFunLessThan)) _) = evalTLess expr
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunGreaterThan)) args) =
--   evalT' $ (HiExprApply $ HiExprValue $ HiValueFunction HiFunAnd) [expr1, expr2] where
--     expr1 = HiExprApply (HiExprValue $ HiValueFunction HiFunNotLessThan) args
--     expr2 = HiExprApply (HiExprValue $ HiValueFunction HiFunNotEquals) args
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunNotLessThan)) args) =
--   evalTNot $ (HiExprApply $ HiExprValue $ HiValueFunction HiFunNot) [expr] where
--     expr = HiExprApply (HiExprValue $ HiValueFunction HiFunLessThan) args
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunNotGreaterThan)) args) =
--   evalTNot $ (HiExprApply $ HiExprValue $ HiValueFunction HiFunNot) [expr] where
--     expr = HiExprApply (HiExprValue $ HiValueFunction HiFunGreaterThan) args

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunLength)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text -> return $ HiValueNumber $ fromIntegral $ T.length text
--       HiValueList list   -> return $ HiValueNumber $ fromIntegral $ S.length list
--       HiValueBytes bs    -> return $ HiValueNumber $ fromIntegral $ BS.length bs
--       _                  -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunLength)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunToUpper)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text -> return $ HiValueString $ T.toUpper text
--       _                  -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunToUpper)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunToLower)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text -> return $ HiValueString $ T.toLower text
--       _                  -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunToLower)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunReverse)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text -> return $ HiValueString $ T.reverse text
--       HiValueList list   -> return $ HiValueList $ S.reverse list
--       HiValueBytes bs    -> return $ HiValueBytes $ BS.reverse bs
--       _                  -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunReverse)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunTrim)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text -> return $ HiValueString $ T.strip text
--       _                  -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunTrim)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueString text)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueNumber n ->
--         do
--           let ind = numerator n
--           when (denominator n /= 1) $ throwError HiErrorInvalidArgument
--           if ind < 0
--             then return HiValueNull
--             else
--               case T.compareLength text (fromIntegral ind) of
--                 GT ->
--                   return $
--                     HiValueString $
--                       T.singleton $
--                         T.index text $
--                           fromIntegral ind
--                 _ -> return HiValueNull
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply val@(HiExprValue (HiValueString text)) [x, y]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueNull -> evalT' (HiExprApply val [HiExprValue $ HiValueNumber 0, y])
--       HiValueNumber n -> do
--         yeval <- evalT' y
--         when (denominator n /= 1) $ throwError HiErrorInvalidArgument
--         case yeval of
--           HiValueNull ->
--             evalT' (HiExprApply
--                       val
--                         [x, HiExprValue $ HiValueNumber $ fromIntegral $ T.length text])
--           HiValueNumber m ->
--             do
--               let len = T.length text
--               (left, right) <- reduceDuplication n m len
--               let (_, t2) = T.splitAt left text
--               let (t1, _) = T.splitAt (right - left) t2
--               return $ HiValueString t1
--           _ -> throwError HiErrorInvalidArgument
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueString _)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueList list)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueNumber n ->
--         do
--           let ind = numerator n
--           when (denominator n /= 1) $ throwError HiErrorInvalidArgument
--           if ind < 0
--             then return HiValueNull
--             else
--               if S.length list > fromIntegral ind
--                 then return $ S.index list (fromIntegral ind)
--                 else return HiValueNull
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply val@(HiExprValue (HiValueList list)) [x, y]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueNull -> evalT' (HiExprApply val [HiExprValue $ HiValueNumber 0, y])
--       HiValueNumber n -> do
--         yeval <- evalT' y
--         when (denominator n /= 1) $ throwError HiErrorInvalidArgument
--         case yeval of
--           HiValueNull ->
--             evalT'
--               (HiExprApply
--                  val
--                    [x, HiExprValue $ HiValueNumber $ fromIntegral $ S.length list])
--           HiValueNumber m ->
--             do
--               let len = S.length list
--               (left, right) <- reduceDuplication n m len
--               let (_, t2) = S.splitAt left list
--               let (t1, _) = S.splitAt (right - left) t2
--               return $ HiValueList t1
--           _ -> throwError HiErrorInvalidArgument
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueList _)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueBytes list)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueNumber n ->
--         do
--           let ind = numerator n
--           when (denominator n /= 1) $ throwError HiErrorInvalidArgument
--           if ind < 0
--             then return HiValueNull
--             else
--               if BS.length list > fromIntegral ind
--                 then return $
--                   HiValueNumber $
--                     toRational $
--                       BS.index list (fromIntegral ind)
--                 else return HiValueNull
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply val@(HiExprValue (HiValueBytes list)) [x, y]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueNull -> evalT' (HiExprApply val [HiExprValue $ HiValueNumber 0, y])
--       HiValueNumber n -> do
--         yeval <- evalT' y
--         when (denominator n /= 1) $ throwError HiErrorInvalidArgument
--         case yeval of
--           HiValueNull ->
--             evalT'
--               (HiExprApply
--                  val
--                    [x, HiExprValue $ HiValueNumber $ fromIntegral $ BS.length list])
--           HiValueNumber m ->
--             do
--               let len = BS.length list
--               (left, right) <- reduceDuplication n m len
--               let (_, t2) = BS.splitAt left list
--               let (t1, _) = BS.splitAt (right - left) t2
--               return $ HiValueBytes t1
--           _ -> throwError HiErrorInvalidArgument
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueBytes _)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunList)) evals) =
--   do
--     values <- traverse evalT' evals
--     return $ HiValueList $ S.fromList values

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunRange)) [x, y]) =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     case xeval of
--       HiValueNumber n -> case yeval of
--         HiValueNumber m ->
--           return $
--             HiValueList $
--               HiValueNumber <$> S.fromList [n..m]
--         _ -> throwError HiErrorInvalidArgument
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunRange)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunFold)) [x, y]) =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     case yeval of
--       HiValueList list ->
--         if S.null list
--           then return HiValueNull
--           else evalT' $
--             foldl1 (\xx yy -> HiExprApply (HiExprValue xeval) [xx, yy]) $
--               map HiExprValue $ toList list
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunFold)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunEncodeUtf8)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text -> return $ HiValueBytes $ encodeUtf8 text
--       _                  -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunEncodeUtf8)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunDecodeUtf8)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueBytes bs -> case decodeUtf8' bs of
--         Left _     -> return HiValueNull
--         Right text -> return $ HiValueString text
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunDecodeUtf8)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunZip)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueBytes bs -> return $ HiValueBytes $ BS.toStrict $
--         compressWith (defaultCompressParams { compressLevel = bestCompression }) $
--           BS.fromStrict bs
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunZip)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunUnzip)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueBytes bs -> return $ HiValueBytes $ BS.toStrict $
--         decompress $
--           BS.fromStrict bs
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunUnzip)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunSerialise)) [x]) =
--   do
--     xeval <- evalT' x
--     return $ HiValueBytes $ BS.toStrict $ serialise xeval
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunSerialise)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunDeserialise)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueBytes bs ->
--         case deserialiseOrFail $ BS.fromStrict bs of
--           Left _  -> throwError HiErrorInvalidArgument
--           Right r -> return r
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunDeserialise)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunPackBytes)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueList list -> HiValueBytes . BS.pack <$> (check . toList) list
--       _                -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunPackBytes)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunUnpackBytes)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueBytes bs ->
--         return $ HiValueList $ S.fromList $
--           map (HiValueNumber . toRational) $ BS.unpack bs
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunUnpackBytes)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunRead)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text -> return $ HiValueAction $ HiActionRead $ T.unpack text
--       _                  -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunRead)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunWrite)) [x, y]) =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     case xeval of
--       HiValueString path ->
--         case yeval of
--           HiValueString text ->
--             return $
--               HiValueAction $
--                 HiActionWrite (T.unpack path) $
--                   encodeUtf8 text
--           _ -> throwError HiErrorInvalidArgument
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunWrite)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunChDir)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text -> return $ HiValueAction $ HiActionChDir $ T.unpack text
--       _                  -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunChDir)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunMkDir)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text -> return $ HiValueAction $ HiActionMkDir $ T.unpack text
--       _                  -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunMkDir)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunRand)) [x, y]) =
--   do
--     xeval <- evalT' x
--     yeval <- evalT' y
--     case xeval of
--       HiValueNumber n1 -> case yeval of
--         HiValueNumber n2 -> return $
--           HiValueAction $
--             HiActionRand
--               (fromIntegral $ numerator n1)
--                 (fromIntegral $ numerator n2)
--         _ -> throwError HiErrorInvalidArgument
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunRand)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunEcho)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text ->
--         return $ HiValueAction $ HiActionEcho text
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunEcho)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprRun r) =
--   do
--     reval <- evalT' r
--     case reval of
--       HiValueAction act -> lift $ runAction act
--       _                 -> throwError HiErrorInvalidArgument

-- evalT' (HiExprApply (HiExprValue (HiValueAction HiActionCwd)) _) =
--   throwError HiErrorInvalidFunction
-- evalT' (HiExprApply (HiExprValue (HiValueAction HiActionNow)) _) =
--   throwError HiErrorInvalidFunction
-- evalT' (HiExprApply (HiExprValue (HiValueAction HiActionRand{})) _) =
--   throwError HiErrorInvalidFunction
-- evalT' (HiExprApply (HiExprValue (HiValueAction HiActionEcho{})) _) =
--   throwError HiErrorInvalidFunction

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunParseTime)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text ->
--         case (readMaybe (T.unpack text) :: Maybe UTCTime) of
--           Nothing -> return HiValueNull
--           Just t  -> return $ HiValueTime t
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunParseTime)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprDict pairs) =
--   do
--     let f (x, y) =
--           do
--             xeval <- evalT' x
--             yeval <- evalT' y
--             return (xeval, yeval)
--     vpairs <- mapM f pairs
--     return $ HiValueDict $ Map.fromList vpairs

-- evalT' (HiExprApply (HiExprValue dict@(HiValueDict _)) [x]) =
--   do
--     xeval <- evalT' x
--     case dict of
--       HiValueDict dic ->
--         case Map.lookup xeval dic of
--           Just val -> return val
--           Nothing  -> return HiValueNull
-- evalT' (HiExprApply (HiExprValue (HiValueDict _)) _) = throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunKeys)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueDict dict ->
--         return $ HiValueList $ S.fromList $ Map.keys dict
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunKeys)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunValues)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueDict dict ->
--         return $ HiValueList $ S.fromList $ Map.elems dict
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunValues)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunCount)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueString text ->
--         return $
--           HiValueDict $
--             Map.map HiValueNumber $
--               Map.fromListWith (+) $ map (\p -> (HiValueString $ T.pack [p], 1)) $
--                 T.unpack text
--       HiValueList list ->
--         return $
--           HiValueDict $
--             Map.map HiValueNumber $
--               Map.fromListWith (+) $ map (, 1) $ toList list
--       bs@(HiValueBytes _) ->
--         do
--           nums <- evalT' $
--             HiExprApply (HiExprValue $ HiValueFunction HiFunUnpackBytes) [HiExprValue bs]
--           case nums of
--             HiValueList list ->
--               return $
--                 HiValueDict $
--                   Map.map HiValueNumber $
--                     Map.fromListWith (+) $ map (, 1) $ toList list
--             _ -> throwError HiErrorInvalidArgument
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunCount)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunInvert)) [x]) =
--   do
--     xeval <- evalT' x
--     case xeval of
--       HiValueDict dict ->
--         return $
--           HiValueDict $
--             Map.map (HiValueList . S.fromList) $
--               Map.fromListWith (<>) pairs where
--                 pairs = [(v, [k]) | (k, v) <- Map.toList dict]
--       _ -> throwError HiErrorInvalidArgument
-- evalT' (HiExprApply (HiExprValue (HiValueFunction HiFunInvert)) _) =
--   throwError HiErrorArityMismatch

-- evalT' (HiExprValue num@(HiValueNumber _)) = return num
-- evalT' (HiExprValue boo@(HiValueBool _)) = return boo
-- evalT' (HiExprValue fun@(HiValueFunction _)) = return fun
-- evalT' (HiExprValue text@(HiValueString _)) = return text
-- evalT' (HiExprValue nil@HiValueNull) = return nil
-- evalT' (HiExprValue bs@(HiValueBytes _)) = return bs
-- evalT' (HiExprValue list@(HiValueList _)) = return list
-- evalT' (HiExprValue act@(HiValueAction _)) = return act
-- evalT' (HiExprValue time@(HiValueTime _)) = return time
-- evalT' (HiExprValue dict@(HiValueDict _)) = return dict
-- evalT' (HiExprApply (HiExprValue (HiValueNumber _)) _) =
--   throwError HiErrorInvalidFunction
-- evalT' (HiExprApply (HiExprValue (HiValueBool _)) _) =
--   throwError HiErrorInvalidFunction
-- evalT' (HiExprApply (HiExprValue HiValueNull) _) =
--   throwError HiErrorInvalidFunction
-- evalT' (HiExprApply app args) =
--   do
--     appeval <- evalT' app
--     evalT' $ HiExprApply (HiExprValue appeval) args

-- check :: HiMonad m => [HiValue] -> ExceptT HiError m [Word8]
-- check [] = return []
-- check (x:xs) =
--   case x of
--     HiValueNumber n ->
--       if n >= 0 && n <= 255
--         then
--           do
--             rest <- check xs
--             return $ fromIntegral (numerator n) : rest
--         else throwError HiErrorInvalidArgument
--     _ -> throwError HiErrorInvalidArgument

-- reduceDuplication :: HiMonad m => Rational -> Rational -> Int -> ExceptT HiError m (Int, Int)
-- reduceDuplication n m len =
--   do
--     let nn = numerator n
--     let left = if nn < 0 then len + fromIntegral nn else fromIntegral nn
--     let mm = numerator m
--     let right = if mm < 0 then len + fromIntegral mm else fromIntegral mm
--     when (denominator m /= 1) $ throwError HiErrorInvalidArgument
--     return (left, right)
