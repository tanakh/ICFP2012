module Main(main) where

import Control.Monad.Trans

import           AI.Common
import qualified Ans as Ans
import           LL
import           DefaultMain

main = defaultMain simpleSolver

simpleSolver :: (MonadIO m) => Solver m
simpleSolver = safetynet $ do
  return $ Ans.Cont 'A'
  