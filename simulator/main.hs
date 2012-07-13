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
  let Opt.AnswerFile ansfn = Opt.answer opt
  forkIO $ ansProvider ansfn ansMVar
  print =<< simulate bd ansMVar

ansProvider :: FilePath -> MVar Ans.Ans ->  IO ()
ansProvider fn mvar = do
  mvs <- filter (`elem` "LRUDWA") <$> readFile fn
  mapM_ (putMVar mvar) $ map Ans.Cont mvs
  putMVar mvar Ans.End