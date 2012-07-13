module DefaultMain(defaultMain) where

import Control.Concurrent
import System.IO

import           LL
import qualified Option as Opt
import           Provider
import qualified Flood

defaultMain :: Solver IO -> IO ()
defaultMain theSolver = do
  opt <- Opt.parseIO
  txt <- case Opt.input opt of
    Opt.Stdin -> getContents
    Opt.InputFile fn -> readFile fn
  let (txtB,txtM) = span (/="") $ lines txt
      bd0 = reverse txtB
      w = maximum $ map length bd0
      bd = map (take w . (++ repeat ' ')) bd0
      fld = Flood.readFlood $ drop 1 txtM
  ansMVar <- newEmptyMVar
  stateMVar <- newEmptyMVar
  solver <- case Opt.answer opt of
    Opt.AnswerFile ansfn -> do
      forkIO $ fileProvider ansfn ansMVar stateMVar
      return $ providedSolver ansMVar stateMVar
    Opt.Keyboard -> do
      hSetBuffering stdout NoBuffering
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      forkIO $ kbdProvider ansMVar stateMVar
      return $ providedSolver ansMVar stateMVar
    Opt.Auto -> do
      return $ theSolver

  print =<< simulate opt fld bd solver
