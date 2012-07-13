import Control.Applicative
import Control.Concurrent
import System.Environment
import System.IO

import qualified Option as Opt
import LL
import qualified Ans as Ans

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
  provider <- case Opt.answer opt of
    Opt.AnswerFile ansfn -> return $ fileProvider ansfn ansMVar
    Opt.Auto -> return $ autoProvider ansMVar
    Opt.Keyboard -> do
      hSetBuffering stdout NoBuffering
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      return $ kbdProvider ansMVar
  let 
  forkIO $ provider
  print =<< simulate bd ansMVar

fileProvider :: FilePath -> MVar Ans.Ans ->  IO ()
fileProvider fn mvar = do
  mvs <- filter (`elem` "LRUDWA") <$> readFile fn
  mapM_ (putMVar mvar) $ map Ans.Cont mvs
  putMVar mvar Ans.End
  
autoProvider :: MVar Ans.Ans -> IO ()  
autoProvider mv = putMVar mv Ans.End                

kbdProvider :: MVar Ans.Ans -> IO ()  
kbdProvider mv = putMVar mv Ans.End                

