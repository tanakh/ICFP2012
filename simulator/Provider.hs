module Provider (
  Provider,
  kbdProvider,
  fileProvider,
  providedSolver,
  blackhole
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Concurrent

import qualified Ans as Ans
import LL

providedSolver :: (MonadIO m) => MVar Ans.Ans -> MVar LLState -> Solver m
providedSolver mvAns mvState = do
  st <- get
  liftIO $ putMVar mvState st
  liftIO $ takeMVar mvAns

type Provider = MVar Ans.Ans -> MVar LLState -> IO ()


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
      | str == "s"           = [Ans.Cont 'S']
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
