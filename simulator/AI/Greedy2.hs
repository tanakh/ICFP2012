{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Loop
import Control.Monad.Trans
import Data.Lens
import Data.List
import qualified Data.PQueue.Min as Q
import Data.IORef
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

data Value =  Happy Int | Afraid Int | Unknown | Danger deriving (Eq, Ord, Show)
data GrandValue = 
  GrandValue
  {expectedScore :: Int,
   gameHasEnded :: Bool,
   playValue :: Value
  }deriving (Eq, Show)

grandValue n = GrandValue n True (Happy 0)

instance Ord GrandValue where
-- the smaller, the better
  compare (GrandValue ex1 end1 val1) (GrandValue ex2 end2 val2) 
    | ex1 /= ex2 = compare ex2 ex1
    | end1 /= end2 = compare end1 end2
    | otherwise = compare val1 val2
  
val2char :: Value -> Char
val2char Unknown = '?'
val2char Danger  = '!'
val2char (Happy n)
  | n <= 9 = head $ show n 
  | True   = toEnum $ (fromEnum 'a' + n-10)
val2char (Afraid n)
  | True   = toEnum $ (fromEnum 'A' + n)

valPlus m (Happy n) = Happy $ m+n 
valPlus m (Afraid n) = Afraid $ m+n 
valPlus _ x = x

valAfraid (Happy m) = Afraid m
valAfraid x = x

greedySolver = safetynet $ do
  _ <- evaluatePlaying True
  yomi <- sort <$> mapM (\c -> (,c) <$> evaluateHand 3 c) "LRUDA"
  liftIO $ print yomi
  let (_, cmd) = head yomi 
  return $ Ans.Cont cmd

evaluateHand :: (Functor m, MonadIO m) => Int -> Char -> LLT m GrandValue
evaluateHand depth hand 
  | depth <= 0 = evaluatePlaying False
  | otherwise = do
    withBackup $ do
      roboPos0 <- access llPosL                
      result0 <- simulateStep hand
      roboPos1 <- access llPosL                
      let result 
            | roboPos0 == roboPos1 = Dead 0
            | otherwise            = result0
      case result of
        Win n -> return $ grandValue n
        Abort n -> return $ grandValue n
        Dead n -> return $ grandValue n
        Skip -> return $ grandValue (-9999999)
        Cont -> 
          head . sort <$> mapM (evaluateHand (depth-1)) "LRUDA"

dijkstra guide initializer preModifier cond = do
  bd <- access llBoardL    
  probes <- liftIO $ newIORef $ Q.empty  
  loopPos $ \ r -> do
    (writes,queues) <- initializer r
    forM_ writes $ \x -> writePos guide r x 
    forM_ queues $ \x -> liftIO $ modifyIORef probes $ Q.insert (x, r)     

  while (liftIO ((not . Q.null) <$> readIORef probes)) $ do
    (val0, r) <- liftIO $ Q.findMin <$> readIORef probes
    liftIO $ modifyIORef probes Q.deleteMin
    val <- preModifier r val0
    oldVal <- head <$> readPosList guide r
    
    when (cond val val0)$ do
      writePos guide r val
      forM_ directions $ \ (_, dr) -> do
        let nr = r + dr
        newVals <- readPosList guide nr
        forM_ newVals $ \ newVal -> do
          liftIO $ modifyIORef probes $ Q.insert (valPlus 1 val, nr)

evaluatePlaying :: (Functor m, MonadIO m) => Bool -> LLT m GrandValue
evaluatePlaying debugFlag = do
  bd <- access llBoardL  
  (w,h) <- getSize
  guide <- liftIO $ VM.replicateM h $ VM.replicate w Unknown

  let initializer r = do
        a <- readPos bd r
        case () of
         _ | a `elem` "O\\" -> return ([], [Happy 0])
           | a `elem` " .R" -> return ([],[])
           | otherwise      -> return ([Danger],[])
      preModifier r val0 = do
        ch <- readPos bd (r + Pos 0 1)
        let val 
              | ch == '*' = valAfraid val0
              | otherwise = val0
        return val
      cond val val0
  dijkstra guide initializer

  when debugFlag $ do
    withBackup $ do
      bd <- access llBoardL
      loopPos $ \r -> do
        val <- head <$> readPosList guide r
        writePos bd r (val2char val)
      showBoard   
  
  remaining <- access llLambdasL        
  totaling <- access llTotalLambdasL
  roboPos <- access llPosL          
  roboVal <- head <$> readPosList guide roboPos
  winScore0 <- abortScore
  abortScore0 <- abortScore
  
  if roboVal < Unknown
    then return $ GrandValue winScore0 False roboVal        
    else return $ GrandValue abortScore0 False Danger
