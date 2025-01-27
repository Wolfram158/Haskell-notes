{-# LANGUAGE LambdaCase #-}

module HW5.Action
  ( HiPermission (..)
  , PermissionException (..)
  , HIO (..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (ap, msum)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Functor (($>))
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (getCurrentTime)
import HW5.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory
import System.Random (randomRIO)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

data PermissionException =
  PermissionRequired HiPermission deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set.Set HiPermission -> IO a }

instance Functor HIO where
  fmap f hio = HIO $ fmap f . runHIO hio

instance Applicative HIO where
  pure x = HIO $ const $ pure x
  (<*>) = ap

instance Monad HIO where
  HIO run >>= f = HIO $ \shi ->
    do
      a <- run shi
      runHIO (f a) shi

instance MonadIO HIO where
  liftIO io = HIO $ const io

instance HiMonad HIO where
  runAction =
    \case
      HiActionRead path ->
        check
          [AllowRead] $
            do
              dfe <- liftIO $ doesFileExist path
              if dfe
                then
                  do
                    content <- liftIO $ readFile path
                    let bs = BS.pack $ map (toEnum . fromEnum) content
                    return $ case decodeUtf8' bs of
                      Left _ -> HiValueBytes bs
                      _      -> HiValueString $ T.pack content
                else
                  liftIO $ HiValueList . S.fromList . map (HiValueString . T.pack)
                    <$> listDirectory path
      HiActionWrite path bs ->
        check
          [AllowWrite] $
            liftIO (BS.writeFile path bs) $> HiValueNull
      HiActionCwd ->
        check
          [AllowRead] $
            HiValueString . T.pack <$> liftIO getCurrentDirectory
      HiActionChDir path ->
        check
          [AllowRead] $
            liftIO (setCurrentDirectory path) $> HiValueNull
      HiActionMkDir path ->
        check
          [AllowWrite] $
            liftIO (createDirectory path) $> HiValueNull
      HiActionNow ->
        check
          [AllowTime] $
            HiValueTime <$> liftIO getCurrentTime
      HiActionRand l r ->
        liftIO $
          HiValueNumber . fromIntegral <$> randomRIO (l, r)
      HiActionEcho text ->
        check
          [AllowWrite] $
            liftIO $ print text $> HiValueNull

check :: [HiPermission] -> HIO HiValue -> HIO HiValue
check perms (HIO run) =
  HIO {
    runHIO =
      \sperm ->
        if all (`Set.member` sperm) perms
          then run sperm
          else msum $ map (throwIO . PermissionRequired) perms
  }
