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
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import System.IO

import           AI.Common
import           LL
import           Pos

printe :: (MonadIO m, Show a) => a -> m ()
printe = liftIO . hPutStrLn stderr . show

newF :: (U.Unbox a, MonadIO m) => a -> LLT m (Field a)
newF initVal = do
  (w,h) <- getSize
  liftIO $ GM.replicateM h $ GM.replicate w initVal

unsafeReadFIO :: U.Unbox a => Field a -> Pos -> IO a
unsafeReadFIO field (Pos x y) = do
  line <- GM.unsafeRead field y
  GM.unsafeRead line x

unsafeWriteFIO :: U.Unbox a => Field a -> Pos -> a -> IO ()
unsafeWriteFIO field (Pos x y) val = do
  line <- GM.unsafeRead field y
  GM.unsafeWrite line x val

unsafeReadF :: (U.Unbox a, MonadIO m)=>Field a -> Pos -> m a
unsafeReadF bd r = liftIO $ unsafeReadFIO bd r

unsafeWriteF :: (U.Unbox a, MonadIO m)=>Field a -> Pos -> a -> m ()
unsafeWriteF bd r val = liftIO $ unsafeWriteFIO bd r val

readFList :: (MonadIO m, U.Unbox a) => Field a -> Pos -> m [a]
readFList bd r = liftIO $ whenInBoundPos bd r [] ((:[]) <$> unsafeReadF bd r)

writeF :: (MonadIO m, U.Unbox a) => Field a -> Pos -> a -> m ()
writeF bd r val = liftIO $ whenInBoundPos bd r () (unsafeWriteF bd r val)

convertF :: (U.Unbox a, U.Unbox b, MonadIO m)
            => Field a -> (a->b) -> LLT m (Field b)
convertF source f = do
  (w,h) <- getSize
  liftIO $ do
    ret <- GM.new h
    forM_ [0..h-1] $ \ y -> do
      srcLine <- GM.unsafeRead source y
      line <- GM.new w
      forM_ [0..w-1] $ \ x -> do
        a <- GM.unsafeRead srcLine x
        GM.unsafeWrite line x (f a)
      GM.unsafeWrite ret y line
    return ret

updateF :: (U.Unbox a, MonadIO m) => Field a -> (a->a) -> LLT m ()
updateF source f= do
  (w,h) <- getSize
  liftIO $ do
    forM_ [0..h-1] $ \ y -> do
      srcLine <- GM.unsafeRead source y
      forM_ [0..w-1] $ \ x -> do
        a <- GM.unsafeRead srcLine x
        GM.unsafeWrite srcLine x (f a)

showF :: (U.Unbox a, MonadIO m) => (a->String) -> Field a -> LLT m ()
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
  isPassable :: a-> Bool
  isPassable x
    | x == unknown = False
    | x == blocked = False
    | otherwise    = True
  terrainSucc :: a->a
  terrainSucc x = if isPassable x then x+1 else x

instance Terrain Int where
  unknown = minBound-2
  blocked = minBound-1
instance Terrain Double where
  unknown = 8901e35
  blocked = 1341e72
wideShow :: (Terrain a,Show a) =>Int -> a -> String
wideShow width val
  | val == unknown = "?? "
  | val == blocked = ">< "
  | otherwise      = take (width-1) (show2 val) ++ " "
  where
    show2 = cut . show
    cut ('0':'.':xs) = '.':xs
    cut x = x

dijkstra :: (MonadIO m, Functor m, Terrain a, U.Unbox a) =>
            Field a -> String -> String -> a -> LLT m ()
dijkstra field sourceStr passableStr initVal = do
  bd <- access llBoardL
  probes <- liftIO $ newIORef $ Q.empty
  forPos $ \ r -> do
    a <- liftIO $ normalize <$> readPos bd r
    case a of
      _ | a `elem` sourceStr   -> do
           liftIO $ modifyIORef probes $ Q.insert (initVal, r)
           writeF field r unknown
        | a `elem` passableStr -> writeF field r unknown
        | otherwise            -> writeF field r blocked
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
  return ()
    where
      normalize c
        | c `elem` "123456789" = '1'
        | c `elem` "ABCDEFGHI" = 'A'
        | True                 = c
