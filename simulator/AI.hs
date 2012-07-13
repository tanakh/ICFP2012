module AI (autoSolver) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans



import qualified Ans as Ans
import           LL

autoSolver :: (MonadIO m) => Solver m
autoSolver = return $ Ans.Cont 'A'
