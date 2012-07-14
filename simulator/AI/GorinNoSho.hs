{- Takusan Utsuto Jissai Atariyasui. -}
{-# OPTIONS -Wall #-}
module AI.GorinNoSho where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Loop
import Control.Monad.Trans
import Data.Lens
import qualified Data.PQueue.Min as Q
import Data.IORef
import qualified Data.Vector.Mutable as VM
import System.IO

import           AI.Common
import           LL

type Field a = VM.IOVector (VM.IOVector a) 

printe :: (MonadIO m, Show a) => a -> m ()
printe = liftIO . hPutStrLn stderr . show

data Terrain = Passable Int | Unknown | Blocked deriving (Eq, Ord, Show)

terrainSucc :: Terrain -> Terrain
terrainSucc (Passable n) = Passable $ n+1
terrainSucc x            = x

dijkstra :: String -> String -> LLT IO (Field Terrain)
dijkstra sourceStr passableStr = do
  bd <- access llBoardL  
  (w,h) <- getSize
  field <- liftIO $ VM.replicateM h $ VM.replicate w Unknown
  probes <- liftIO $ newIORef $ Q.empty
  loopPos $ \ r -> do
    a <- readPos bd r
    case a of
      _ | a `elem` sourceStr   -> 
           liftIO $ modifyIORef probes $ Q.insert (Passable 1, r) 
        | a `elem` passableStr -> return ()
        | otherwise            -> writePos field r Blocked
  while (liftIO ((not . Q.null) <$> readIORef probes)) $ do
    (val, r) <- liftIO $ Q.findMin <$> readIORef probes
    liftIO $ modifyIORef probes Q.deleteMin
    oldVal <- head <$> readPosList field r
    when (val < oldVal && oldVal /= Blocked) $ do
      writePos field r val
      forM_ directions $ \ (_, dr) -> do
        let nr = r + dr
        newVals <- readPosList field nr
        forM_ newVals $ \ _ -> do
          liftIO $ modifyIORef probes $ Q.insert (terrainSucc val, nr)
  return field



