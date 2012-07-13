module AI (autoProvider) where

import Control.Applicative
import Control.Monad
import Control.Concurrent


import qualified Ans as Ans
import           LL
import           Provider

autoProvider :: Provider
autoProvider mvAns mvState = do
  forkIO $ blackhole mvState
  replicateM_ 100 $ putMVar mvAns $ Ans.Cont 'D'
  putMVar mvAns Ans.End
