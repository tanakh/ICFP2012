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


printe :: (MonadIO m, Show a) => a -> m ()
printe = liftIO . hPutStrLn stderr . show

yomiDepth = 3
debugMode = False

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
  _ <- evaluatePlaying debugMode
  yomi <- sort <$> mapM (\c -> (,c) <$> evaluateHand yomiDepth c) "LRUDA"
  when debugMode $ printe $ yomi
  let (_, cmd) = head yomi
  return $ Ans.Cont cmd

evaluateHand :: (Functor m, MonadIO m) => Int -> Char -> LLT m GrandValue
evaluateHand depth hand
  | depth <= 0 = evaluatePlaying False
  | otherwise = do
    withBackup $ do
      roboPos0 <- access llPosL
      result <- simulateStep hand
      roboPos1 <- access llPosL
      case result of
        Win n -> return $ grandValue n
        Abort n -> do 
          return $ grandValue n
        Dead n -> return $ grandValue n
        Cont -> 
          head . sort <$> mapM (evaluateHand (depth-1)) "LRUDA"

dijkstra guide = do
  bd <- access llBoardL    
  probes <- liftIO $ newIORef $ Q.empty
  loopPos $ \ r -> do
    a <- readPos bd r
    case a of
      _ | a == '\\' || a == 'O' -> do
           liftIO $ modifyIORef probes $ Q.insert (Happy 0, r)
        | a == '.' || a == ' ' || a == 'R' -> return ()
        | otherwise -> writePos guide r Danger
  while (liftIO ((not . Q.null) <$> readIORef probes)) $ do
    (val0, r) <- liftIO $ Q.findMin <$> readIORef probes
    liftIO $ modifyIORef probes Q.deleteMin
    ch <- readPos bd (r + Pos 0 1)
    let val
          | ch == '*' = valAfraid val0
          | otherwise = val0
    oldVal <- head <$> readPosList guide r
    when (val < oldVal && oldVal /= Danger) $ do
      writePos guide r val
      forM_ directions $ \ (_, dr) -> do
        let nr = r + dr
        newVals <- readPosList guide nr
        forM_ newVals $ \ newVal -> do
          liftIO $ modifyIORef probes $ Q.insert (valPlus 1 val, nr)

dijkstraRobot guideRobot = do
  bd <- access llBoardL    
  probes <- liftIO $ newIORef $ Q.empty
  loopPos $ \ r -> do
    a <- readPos bd r
    case a of
      _ | a == 'R' -> do
           liftIO $ modifyIORef probes $ Q.insert (Happy 0, r) 
        | a `elem` " .R\\OL"-> return ()
        | otherwise -> writePos guideRobot r Danger
  while (liftIO ((not . Q.null) <$> readIORef probes)) $ do
    (val, r) <- liftIO $ Q.findMin <$> readIORef probes
    liftIO $ modifyIORef probes Q.deleteMin
    oldVal <- head <$> readPosList guideRobot r
    when (val < oldVal && oldVal /= Danger) $ do
      writePos guideRobot r val
      forM_ directions $ \ (_, dr) -> do
        let nr = r + dr
        newVals <- readPosList guideRobot nr
        forM_ newVals $ \ newVal -> do
          liftIO $ modifyIORef probes $ Q.insert (valPlus 1 val, nr)




evaluatePlaying :: (Functor m, MonadIO m) => Bool -> LLT m GrandValue
evaluatePlaying debugFlag = do
  bd <- access llBoardL  
  (w,h) <- getSize
  guide <- liftIO $ VM.replicateM h $ VM.replicate w Unknown
  guideRobot <- liftIO $ VM.replicateM h $ VM.replicate w Unknown
  dijkstra guide 
  dijkstraRobot guideRobot

  when debugFlag $ do
    withBackup $ do
      bd <- access llBoardL
      loopPos $ \r -> do
        val <- head <$> readPosList guide r
        writePos bd r (val2char val)
      showBoard   
  when debugFlag $ do
    withBackup $ do
      bd <- access llBoardL
      loopPos $ \r -> do
        val <- head <$> readPosList guideRobot r
        writePos bd r (val2char val)
      showBoard   

  remaining <- access llLambdasL        
  totaling <- access llTotalLambdasL
  roboPos <- access llPosL          
  liftPos <- access llLiftPosL          
  roboVal <- head <$> readPosList guide roboPos
  liftVal <- head <$> readPosList guideRobot liftPos


  winScore0 <- abortScore
  abortScore0 <- abortScore

  yesLambdaR <- liftIO $ newIORef 0
  noLambdaR  <- liftIO $ newIORef 0
  loopPos $ \r -> do
    c <- readPos bd r
    when (c /= '\\') continue
    val <- head <$> readPosList guideRobot r
    if (val < Unknown)
       then liftIO $ modifyIORef yesLambdaR  (1+)
       else liftIO $ modifyIORef noLambdaR   (1+)
  yesLambda <- liftIO $ readIORef yesLambdaR
  noLambda <- liftIO $ readIORef noLambdaR
  let liftYes :: Bool 
      liftYes = liftVal < Unknown && noLambda == 0
      abortScoreHope = abortScore0 + 25 * yesLambda
      winScoreHope = winScore0 + 37 * yesLambda + 100
  case () of
    _ | not liftYes          -> return $ GrandValue abortScoreHope False roboVal
      | roboVal   >= Unknown -> return $ GrandValue abortScore0 False roboVal
      | otherwise            -> return $ GrandValue winScoreHope False roboVal
 

