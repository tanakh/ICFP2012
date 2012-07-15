module DefaultMain (defaultMain) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5
import System.Exit
import System.IO
import System.Cmd
import System.FilePath
import Text.Printf
import System.Posix.Signals
import System.Posix.Process


import qualified AI.Oracle as Oracle
import           LL
import qualified Option
import           Provider
import qualified Flood


defaultMain :: Oracle.Oracle -> Solver IO -> IO ()
defaultMain oracle theSolver = do
  opt <- Option.parseIO
  txt <- case Option.input opt of
    Option.Stdin -> getContents
    Option.InputFile fn -> readFile fn
  ansMVar <- newEmptyMVar
  stateMVar <- newEmptyMVar
  
  myProcID <- getProcessID
  let handler = do
       Tejun _ _ rep <- atomically $ readTVar (Oracle.tejunVar oracle)
       putStrLn rep
       signalProcess sigKILL myProcID
  installHandler sigINT (Catch handler) Nothing

  solver <- case Option.answer opt of
    Option.AnswerFile ansfn -> do
      forkIO $ fileProvider ansfn ansMVar stateMVar
      return $ providedSolver ansMVar stateMVar
    Option.Keyboard -> do
      hSetBuffering stdout NoBuffering
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      forkIO $ kbdProvider ansMVar stateMVar
      return $ providedSolver ansMVar stateMVar
    Option.Auto -> do
      return $ theSolver

  (res, score, replay) <-
    simulate (Option.verbose opt) txt solver

  when (Option.verbose opt) $ do
    printf "%s %d: %s\n" (show res) score replay
  Oracle.submit oracle $ Tejun score res replay
  Oracle.save oracle  

  case Option.replay opt of
    Option.ReplayNothing -> do
      return ()
    Option.ReplayDefault -> do
      let fn = case Option.input opt of
            Option.InputFile fp -> fp
            Option.Stdin -> "STDIN"
      system "mkdir -p replay/"
      writeFile
        (printf "replay/%s-%d-%s.txt"
         (dropExtension $ takeFileName fn)
         score
         (take 6 $ show $ md5 $ L.pack replay))
        replay

  when (Option.verbose opt) $ do
    putStrLn replay
