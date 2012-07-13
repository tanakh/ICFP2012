import Control.Applicative
import Control.Monad
import Control.Concurrent
import System.Environment
import System.IO

import           AI
import qualified Ans as Ans
import           LL
import qualified Option as Opt
import           Provider


main :: IO ()
main = do
  opt <- Opt.parseIO
  txt <- case Opt.input opt of
    Opt.Stdin -> getContents
    Opt.InputFile fn -> readFile fn
  let bd0 = reverse $ lines txt
      w = maximum $ map length bd0
      bd = map (take w . (++ repeat ' ')) bd0
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
      return $ autoSolver
      
  print =<< simulate opt bd solver

