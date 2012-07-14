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
import           Pos

printe :: (MonadIO m, Show a) => a -> m ()
printe = liftIO . hPutStrLn stderr . show

type Field a = VM.IOVector (VM.IOVector a) 

unsafeReadFIO :: Field a -> Pos -> IO a
unsafeReadFIO field (Pos x y) = do
  line <- VM.unsafeRead field y
  VM.unsafeRead line x

unsafeWriteFIO :: Field a -> Pos -> a -> IO ()
unsafeWriteFIO field (Pos x y) val = do
  line <- VM.unsafeRead field y
  VM.unsafeWrite line x val

unsafeReadF :: (MonadIO m)=>Field a -> Pos -> m a
unsafeReadF bd r = liftIO $ unsafeReadFIO bd r

unsafeWriteF :: (MonadIO m)=>Field a -> Pos -> a -> m ()
unsafeWriteF bd r val = liftIO $ unsafeWriteFIO bd r val

readFList :: (Functor m, MonadIO m) => Field a -> Pos -> m [a]
readFList bd r = whenPosInBound bd r (return []) ((:[]) <$> unsafeReadF bd r)

writeF :: (MonadIO m) => Field a -> Pos -> a -> m ()
writeF bd r val = whenPosInBound bd r (return ()) (unsafeWriteF bd r val)

convertF :: (MonadIO m ) => Field a -> (a->b) -> LLT m (Field b)
convertF source f = do
  (w,h) <- getSize
  liftIO $ do
    ret <- VM.new h 
    forM_ [0..h-1] $ \ y -> do
      srcLine <- VM.unsafeRead source y
      line <- VM.new w
      forM_ [0..w-1] $ \ x -> do
        a <- VM.unsafeRead srcLine x
        VM.unsafeWrite line x (f a)
      VM.unsafeWrite ret y line
    return ret

updateF :: (MonadIO m ) => Field a -> (a->a) -> LLT m ()
updateF source f= do
  (w,h) <- getSize
  liftIO $ do
    forM_ [0..h-1] $ \ y -> do
      srcLine <- VM.unsafeRead source y
      forM_ [0..w-1] $ \ x -> do
        a <- VM.unsafeRead srcLine x
        VM.unsafeWrite srcLine x (f a)

showF :: (MonadIO m ) => (a->String) -> Field a -> LLT m ()
showF convert bd = do
  (w,h) <- getSize
  liftIO $ do
    maxLenRef <- newIORef 0
    forM_ [h-1, h-2 .. 0] $ \y -> do
      forM_ [0..w-1] $ \x -> do
        a <- unsafeReadFIO bd (Pos x y)
        modifyIORef maxLenRef (\b -> max b $  length $ convert a)
    maxLen <- readIORef maxLenRef
    forM_ [h-1, h-2 .. 0] $ \y -> do
      forM_ [0..w-1] $ \x -> do
        a <- unsafeReadFIO bd (Pos x y)
        let str = convert a 
            len = length str
            str2 = (replicate (maxLen-len) ' ') ++ str
        hPutStr stderr str2
      hPutStr stderr "\n"


class (Eq a, Ord a, Num a) => Terrain a where
  unknown :: a
  blocked :: a
  terrainSucc :: a->a
  terrainSucc x
    | x == unknown = x
    | x == blocked = x
    | otherwise    = x+1

instance Terrain Int where
  unknown = minBound-2
  blocked = minBound-1
instance Terrain Double where
  unknown = 1/8901e35
  blocked = 1/1341e72

dijkstra :: (MonadIO m, Terrain a) => String -> String -> a -> LLT m (Field a)
dijkstra sourceStr passableStr initVal = do
  bd <- access llBoardL  
  (w,h) <- getSize
  field <- liftIO $ VM.replicateM h $ VM.replicate w unknown
  probes <- liftIO $ newIORef $ Q.empty
  loopPos $ \ r -> do
    a <- readPos bd r
    case a of
      _ | a `elem` sourceStr   -> 
           liftIO $ modifyIORef probes $ Q.insert (initVal, r) 
        | a `elem` passableStr -> return ()
        | otherwise            -> liftIO $ unsafeWriteFIO field r blocked
  while (liftIO ((not . Q.null) <$> readIORef probes)) $ do
    (val, r) <- liftIO $ Q.findMin <$> readIORef probes
    liftIO $ modifyIORef probes Q.deleteMin
    oldVal <- unsafeReadF field r
    when (oldVal == unknown || (val < oldVal && oldVal /= blocked)) $ do
      writeF field r val
      forM_ directions $ \ (_, dr) -> do
        let nr = r + dr
        newVals <- readFList field nr
        forM_ newVals $ \ _ -> do
          liftIO $ modifyIORef probes $ Q.insert (terrainSucc val, nr)
  return field



