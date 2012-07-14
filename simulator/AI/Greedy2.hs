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
data GrandValue = WinGame Int
                | Playing Int Value
                | AbortGame Int
                | StuckGame
                | DeadGame Int deriving (Eq, Ord, Show)

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
  step <- access llStepL
  if step > 200 then return $ Ans.Cont 'A'
    else do
    _ <- evaluatePlaying True
    yomi <- sort <$> mapM (\c -> (,c) <$> evaluateHand 3 c) "LRUDA"
    -- liftIO $ print yomi
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
        Win n -> return $ WinGame (-n)
        Abort n -> return $ AbortGame (-n)
        Dead n -> return $ DeadGame (-n)
        Skip -> return $ DeadGame 99999999
        Cont ->
          head . sort <$> mapM (evaluateHand (depth-1)) "LRUDA"

evaluatePlaying :: (Functor m, MonadIO m) => Bool -> LLT m GrandValue
evaluatePlaying debugFlag = do
  bd <- access llBoardL
  (w,h) <- getSize
  guide <- liftIO $ VM.replicateM h $ VM.replicate w Unknown
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

  when (False && debugFlag) $ do
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
  if roboVal < Unknown
    then return $ Playing (totaling-remaining) roboVal
    else return $ StuckGame
