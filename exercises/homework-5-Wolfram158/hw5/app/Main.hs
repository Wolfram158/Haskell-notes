module Main (main) where

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import qualified Data.Set as Set
import HW5.Action (HIO (..), HiPermission (..))
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyError, prettyValue)
import Prettyprinter (Doc, defaultLayoutOptions, hardline, layoutSmart)
import Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import System.Console.Haskeline (InputT, Settings (..), defaultSettings, getHistory, getInputLine,
                                 outputStrLn, putHistory, runInputT)
import System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe, historyLines)
import System.IO (stdout)
import Text.Megaparsec.Error (errorBundlePretty)

printPretty :: (a -> Doc AnsiStyle) -> a -> IO ()
printPretty prettifier x = renderIO stdout $
  layoutSmart defaultLayoutOptions $
    prettifier x <> hardline

insertAllow, deleteAllow :: HiPermission -> InputT (StateT (Set.Set HiPermission) IO) ()
insertAllow perm = lift $ modify $ Set.insert perm
deleteAllow perm = lift $ modify $ Set.delete perm

main :: IO ()
main = evalStateT (runInputT defaultSettings {autoAddHistory = False} loop) Set.empty
   where
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input ->
                 do
                  case words input of
                    [":history"] ->
                      do
                        history <- getHistory
                        outputStrLn $ unlines $ historyLines history
                    [":set", "+read"] -> insertAllow AllowRead
                    [":set", "-read"] -> deleteAllow AllowRead
                    [":set", "+write"] -> insertAllow AllowWrite
                    [":set", "-write"] -> deleteAllow AllowWrite
                    [":set", "+time"] -> insertAllow AllowTime
                    [":set", "-time"] -> deleteAllow AllowTime
                    _ ->
                      case parse input of
                        Left bundle -> outputStrLn (errorBundlePretty bundle)
                        Right expr ->
                          do
                            allows <- lift get
                            res <- liftIO $ runHIO (eval expr) allows
                            case res of
                              Left err     -> liftIO $ printPretty prettyError err
                              Right result -> liftIO $ printPretty prettyValue result
                  history <- getHistory
                  putHistory $ addHistoryUnlessConsecutiveDupe input history
                  loop
