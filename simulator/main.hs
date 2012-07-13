import Control.Applicative
import Control.Monad
import Control.Concurrent
import System.Environment
import System.IO

import qualified Option as Opt
import LL
import qualified Ans as Ans

type Provider = MVar Ans.Ans -> MVar LLState -> IO ()

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
  provider <- case Opt.answer opt of
    Opt.AnswerFile ansfn -> return $ fileProvider ansfn ansMVar stateMVar
    Opt.Auto -> return $ autoProvider ansMVar stateMVar
    Opt.Keyboard -> do
      hSetBuffering stdout NoBuffering
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      return $ kbdProvider ansMVar stateMVar
  forkIO $ provider
  print =<< simulate opt bd ansMVar stateMVar

blackhole :: MVar a -> IO ()
blackhole mv = forever $ do
  _ <- takeMVar mv
  return ()

fileProvider :: FilePath -> Provider
fileProvider fn mvAns mvState = do
  forkIO $ blackhole mvState
  mvs <- filter (`elem` "LRUDWA") <$> readFile fn
  mapM_ (putMVar mvAns) $ map Ans.Cont mvs
  putMVar mvAns Ans.End

autoProvider :: Provider
autoProvider mvAns mvState = do
  forkIO $ blackhole mvState
  replicateM_ 100 $ putMVar mvAns $ Ans.Cont 'D'
  putMVar mvAns Ans.End

kbdProvider :: Provider
kbdProvider mvAns mvState = forever $ do
  forkIO $ blackhole mvState
  str <- getCharWithEsc
  forM_ (parse str (map fromEnum str)) $ \ans ->
    putMVar mvAns ans
  where
    parse str codes
      | str == "h"           = [Ans.Cont 'L']
      | str == "j"           = [Ans.Cont 'D']
      | str == "k"           = [Ans.Cont 'U']
      | str == "l"           = [Ans.Cont 'R']
      | str == "."           = [Ans.Cont 'W']
      | str == " "           = [Ans.Cont 'W']
      | str == "q"           = [Ans.Cont 'A']
      | str == "u"           = [Ans.Undo]
      | codes == [8]         = [Ans.Undo]
      | codes == [27,91,65]  = [Ans.Cont 'U']
      | codes == [27,91,66]  = [Ans.Cont 'D']
      | codes == [27,91,67]  = [Ans.Cont 'R']
      | codes == [27,91,68]  = [Ans.Cont 'L']
      | otherwise            = []
    getCharWithEsc = do
      a <- getChar
      if a /= toEnum 27
        then return [a]
        else do
          b <- getChar
          c <- getChar
          return [a,b,c]
