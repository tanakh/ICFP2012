module AI (autoSolver) where

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

import qualified Ans as Ans
import           LL
import           Pos

directions = [('L', Pos (-1) 0), 
              ('R', Pos 1 0), 
              ('U', Pos 0 1),
              ('D', Pos 0 (-1))
            ]

atomift :: (MonadIO m) => STM a -> m a
atomift = liftIO . atomically 

autoSolver :: (MonadIO m) => Solver m
autoSolver = withBackup $ do
  bd <- access llBoardL  
  guide <- liftIO $ V.unsafeThaw =<< V.mapM UM.clone =<< V.unsafeFreeze bd  
  let h = GM.length bd
  w <- liftIO $ GM.length <$> GM.read bd 0
  
  probes <- atomift $ newTQueue
  foreach [0..h-1] $ \y -> do
    foreach [0..w-1] $ \x -> do
      let r = Pos x y
      a <- readPos bd r
      when (a == '\\' || a == 'O') $ do
        writePos guide r 'W'        
        atomift $ writeTQueue probes r
        continue
      when (a == '.' || a == ' ' || a == 'R') $ do      
        writePos guide r ' '
        continue
      writePos guide r 'A'        
  while (atomift (not <$> isEmptyTQueue probes)) $ do
    r <- atomift $ readTQueue probes
    foreach directions $ \ (symbol, dr) -> do
      let nr = r - dr
      nc <- readPos guide nr
      when (nc==' ') $ do
        writePos guide nr symbol
        atomift $ writeTQueue probes nr
  withBackup $ do
    llBoardL ~= guide
    --showBoard
  roboPos <- access llPosL
  cmd <- readPos guide roboPos
  return $ Ans.Cont cmd
