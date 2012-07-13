module Main (main) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Loop
import Control.Monad.Trans
import Data.Lens
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

import System.IO

import           AI.Common
import qualified Ans as Ans
import           LL
import           Pos
import           DefaultMain



main = defaultMain greedySolver

data Value =  Happy Int | Unknown | Danger deriving (Eq, Ord)

val2char :: Value -> Char
val2char Unknown = '?'
val2char Danger  = '!'
val2char (Happy n)
  | n <= 9 = show n 
  
valPlus m (Happy n) = Happy $ m+n 
valPlus _ x = x

atomift :: (MonadIO m) => STM a -> m a
atomift = liftIO . atomically 

greedySolver :: (MonadIO m) => Solver m
greedySolver = safetynet $ do
  bd <- access llBoardL  
  (w,h) <- getSize
  guide <- VM.replicateM h $ VM.replicate w Unknown
  probes <- atomift $ newTQueue
  loopPos $ \ r -> do
    a <- readPos bd r
    case a of
      _ | a == '\\' || a == 'O' -> do
           writePos guide r (Happy 0)        
           atomift $ writeTQueue probes (r, Happy 0)
        | a == '.' || a == ' ' || a == 'R' -> 
           writePos guide r Danger
        | otherwise -> writePos guide r Unknown         
  while (atomift (not <$> isEmptyTQueue probes)) $ do
    (r, val) <- atomift $ readTQueue probes
    forM_ directions $ \ (_, dr) -> do
      let nr = r + dr
      newVals <- readPosList guide nr
      forM_ newVals $ \ newVal -> do
        when (newVal==Unknown) $ do
          writePos guide nr val
          atomift $ writeTQueue probes (nr, succ val)
  withBackup $ do
    bd <- access llBoardL
    loopPos $ \r -> do
      val <- readPos guide r
      writePos bd (val2char val)
    showBoard 
  roboPos <- access llPosL
  cmd     <- readPos guide roboPos
  return $ Ans.Cont cmd
