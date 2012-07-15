{-# LANGUAGE TupleSections, ViewPatterns, RecordWildCards, BangPatterns #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Vector.Mutable as VM
import Data.List
import Data.Lens
import Data.Word
import Data.Ord
import System.IO
import System.Random

import Ans
import AI.Common
import qualified AI.Oracle as Oracle
import DefaultMain
import LL
import AI.GorinNoSho
import AI.Oracle
import qualified Option
import AI.Cooking(choose)

minf :: Int
minf = -10^(9::Int)

moves :: [Char]
moves = "LRUDWSA"

type LL = LLT IO

type Cache = IORef (HM.HashMap Word64 [CacheEntry])

data CacheEntry
  = CacheEntry
    { ceStep      :: {-# UNPACK #-} !Int
    , ceWaterStep :: {-# UNPACK #-} !Int
    , ceRazors    :: {-# UNPACK #-} !Int
    }

type History = IORef (HM.HashMap Word64 Int)

readHistory :: History -> Word64 -> IO Int
readHistory hist key = do
  hm <- readIORef hist 
  case HM.lookup key hm of
    Just x  -> return x
    Nothing -> do
      modifyIORef hist $ HM.insert key 0
      return 0

modifyHistory :: History -> Word64 -> (Int -> Int)->IO ()
modifyHistory hist key f = do
  x <- readHistory hist key
  modifyIORef hist $ HM.insert key (f x)


isWorseThan :: CacheEntry -> CacheEntry -> Bool
a `isWorseThan` b =
  ceStep a >= ceStep b
  && ceWaterStep a >= ceWaterStep b
  && ceRazors a <= ceRazors b

addCacheEntry :: IORef (HM.HashMap Word64 [CacheEntry]) -> LLState -> IO Bool
addCacheEntry hmr st = do
  hm <- readIORef hmr
  let ce = CacheEntry (llStep st) (llWaterStep st) (llRazors st)
  let cm = case HM.lookup (llHash st) hm of
        Just cfs -> not $ any (ce `isWorseThan`) cfs
        _ -> True
  when cm $ do
    modifyIORef hmr $ HM.insertWith (++) (llHash st) [ce]
  return cm

goodnessCheck :: History -> Int ->  Char -> LL Int
goodnessCheck hist fuel hand
  | fuel <= 0 = withStep hand $ do
                       res <- access llResultL
                       h <- access llHashL
                       cnt <- liftIO $ readHistory hist h
                       return $ case () of
                                  _ | res == Dead -> -2
                                    | cnt > 0     -> -1
                                    | True        ->  0
  | otherwise = withStep hand $ do
                    gs <- (forM "LRUD" $ goodnessCheck hist (fuel-1))
                    return $ maximum gs
main :: IO ()
main = do
  opt <- Option.parseIO
  valueFieldRef <- newIORef undefined
  let inputfn = case Option.input opt of
            Option.InputFile fp -> fp
            Option.Stdin -> "STDIN"
  oracle <- Oracle.new inputfn
  when (Option.oracleSource opt/= "") $ do
    Oracle.load oracle $ Option.oracleSource opt

  infiniteLoop <- liftIO $ Oracle.ask oracle "infiniteLoop" $ return False
  (if infiniteLoop then forever else id) $ do
    motionWeight <- forM "LRUD" $ \char -> do
      w <- randomRIO (0.1, 3)
      return (char, w)
    history <- newIORef HM.empty
    --- start of One Challenge
    defaultMain oracle $ do
      step <- access llStepL
      (liftIO . Oracle.submit oracle) =<< getAbortTejun
  
      bfDepth <- liftIO $ Oracle.ask oracle "bfDepth" $ return 10
      hmr <- liftIO $ newIORef HM.empty

      hashNow <- access llHashL
      kabutta <- (>2) <$> (liftIO $ readHistory history hashNow)
      liftIO $ modifyHistory history hashNow (1+)  
  
      (mov, sc) <- withBackup $ do
        rs <- forM moves $ \mov -> withStep mov $ do
          -- TODO: unify
          (mov, ) <$> eval undefined hmr 0 bfDepth
        return $ maximumBy (comparing snd) rs
  
      greedyDepth <- liftIO $ Oracle.ask oracle "greedyDepth" $ return 4
      valueField <- if step > 0 then liftIO $ readIORef valueFieldRef
                                else do
                                    ret <- newF (0::Double)
                                    liftIO $ writeIORef valueFieldRef ret
                                    return ret
      dijkstraEX valueField "\\O" " .!\\R" motionWeight 0
      updateF valueField (\x -> max 0 $ 75-x)
      roboPos <- access llPosL
      val <- unsafeReadF valueField roboPos
      (mov2, confidence) <-  do
               cand <- forM "LRUD" $ \hand -> do
                     val3 <- unsafeReadF valueField $ roboPos + hand2pos hand
                     flag <- goodnessCheck history greedyDepth hand
                     return ((flag,val3), hand)
               let top = last $ sort $ cand
               return $ (snd top {-move-}, fst (fst top) {-whether it was safe-})
  
      combineBFFirst <- liftIO $ Oracle.ask oracle "combineBFFirst" $ return True
      perfectGreedy <- liftIO $ Oracle.ask oracle "perfectGreedy" $ return False
      movRand <- liftIO $ choose "RLDU"
      let mov3
           | kabutta                                    = 'A'
           | perfectGreedy                              = mov2
           | combineBFFirst && (mov /= 'A' || val <= 0) = mov
           | combineBFFirst                             = mov2
           | not (combineBFFirst) && (confidence < 0)   = mov2
           | otherwise                                  = mov
  
      when (Option.verbose opt) $ liftIO $ putStrLn $ "score : " ++ show sc ++ ", move: " ++  [mov,mov2]
      return $ Ans.Cont mov3
  

prePruning :: LLState -> Char -> LL Bool
prePruning LLState {..} move
  | move == 'S' && llRazors == 0 =
    return False
  | otherwise =
    return True

pruning :: LLState -> Char -> LLState -> LL Bool
pruning cur move next
  | move `elem` "LRUD" && llPos cur == llPos next =
    return False
  | otherwise =
    return True

staticScore :: Field Int -> LL Int
staticScore _ = do
  LLState {..} <- get
  abt <- abortScore
  return abt
  {-
  aScore <- abortScore
  pos <- access llPosL
  futureScore <- unsafeReadF valueField pos
  step <- access llStepL
  return $ aScore + 0*step + futureScore
  -}

eval :: Field Int -> Cache -> Int -> Int -> LL Int
eval valueField cache !curBest !fuel = do
  mb <- score
  case mb of
    Just sc -> return sc
    _ | fuel <= 0 -> staticScore valueField
    _ -> do
      st <- get
      ok <- liftIO $ addCacheEntry cache st
      if ok
        then do
        best cache moves $ \mov -> do
          -- liftIO $ putStrLn $ "fuel: " ++ show fuel ++ ", mov: " ++ [mov]
          -- showStatus
          eval valueField cache curBest (fuel - 1)
        else return minf

best :: Cache -> [Char] -> (Char -> LL Int) -> LL Int
best cache ls m = do
  res <- forM ls $ \x -> do
    cur <- get
    pp <- prePruning cur x
    if pp
      then do
      withStep x $ do
        next <- get
        p <- pruning cur x next
        if p then m x else return minf
      else return minf
  return $ maximum res
