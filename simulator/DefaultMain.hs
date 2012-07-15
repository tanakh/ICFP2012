module DefaultMain (defaultMain) where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5
import System.IO
import System.Cmd
import System.FilePath
import Text.Printf

import           LL
import qualified Option
import           Provider
import qualified Flood

defaultMain :: Solver IO -> IO ()
defaultMain theSolver = do
  opt <- Option.parseIO
  txt <- case Option.input opt of
    Option.Stdin -> getContents
    Option.InputFile fn -> readFile fn
  let (txtB,txtM) = span (/="") $ lines txt
      bd0 = reverse txtB
      w = maximum $ map length bd0
      bd = map (take w . (++ repeat ' ')) bd0
      fld = Flood.readFlood $ drop 1 txtM
  ansMVar <- newEmptyMVar
  stateMVar <- newEmptyMVar

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
    simulate (Option.verbose opt) fld bd solver

  when (Option.verbose opt) $ do
    printf "%s %d: %s\n" (show res) score replay

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
