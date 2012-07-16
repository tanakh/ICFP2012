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
import qualified Data.HashSet as HS
import Data.Lens
import Data.Word
import Data.Ord
import System.IO

import Ans
import AI.Common
import qualified AI.Oracle as Oracle
import DefaultMain
import LL
import AI.GorinNoSho
import AI.Oracle
import qualified Option

minf :: Int
minf = -10^(9::Int)

type LL = LLT IO

type Cache = IORef (HM.HashMap Word64 [CacheEntry])

data CacheEntry
  = CacheEntry
    { ceStep      :: {-# UNPACK #-} !Int
    , ceWaterStep :: {-# UNPACK #-} !Int
    , ceRazors    :: {-# UNPACK #-} !Int
    }

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

safetyCheck :: HS.HashSet Word64 -> Int ->  Char -> LL Bool
safetyCheck hist fuel hand
  | fuel <= 0 = withStep hand $ do
                       res <- access llResultL
                       h <- access llHashL
                       return $ res /= Dead && not (HS.member h hist)
  | otherwise = withStep hand $
                     or <$> (forM "LRUD" $ safetyCheck hist (fuel-1))
main :: IO ()
main = do
  opt <- Option.parseIO
  historyRef <- newIORef HS.empty
  valueFieldRef <- newIORef undefined
  let inputfn = case Option.input opt of
            Option.InputFile fp -> fp
            Option.Stdin -> "STDIN"
  oracle <- Oracle.new inputfn
  defaultMain oracle $ do
    step <- access llStepL
    (liftIO . Oracle.submit oracle) =<< getAbortTejun

    bfDepth <- liftIO $ Oracle.ask oracle "bfDepth" $ return 10
    hmr <- liftIO $ newIORef HM.empty
    hashNow <- access llHashL
    liftIO $ modifyIORef historyRef $ HS.insert hashNow
    history <- liftIO $ readIORef historyRef
    (mov, sc) <- withBackup $ search undefined hmr 0 bfDepth

    greedyDepth <- liftIO $ Oracle.ask oracle "greedyDepth" $ return 4
    valueField <- if step > 0 then liftIO $ readIORef valueFieldRef
                              else do
                                  ret <- newF (0::Int)
                                  liftIO $ writeIORef valueFieldRef ret
                                  return ret
    dijkstra valueField "\\O" " .!\\R" 0
    updateF valueField (\x -> max 0 $ 75-x)
    roboPos <- access llPosL
    val <- unsafeReadF valueField roboPos
    (mov2, confidence) <-  do
             cand <- forM "LRUD" $ \hand -> do
                   val3 <- unsafeReadF valueField $ roboPos + hand2pos hand
                   flag <- safetyCheck history greedyDepth hand
                   return ((flag,val3), hand)
             let top = last $ sort $ cand
             return $ (snd top {-move-}, fst (fst top) {-whether it was safe-})

    combineBFFirst <- liftIO $ Oracle.ask oracle "combineBFFirst" $ return True
    let mov3
         | combineBFFirst && (mov /= 'A' || val <= 0) = mov
         | combineBFFirst                             = mov2
         | not (combineBFFirst) && confidence         = mov2
         | otherwise                                  = mov

    liftIO $ putStrLn $ "score : " ++ show sc ++ ", move: " ++  [mov,mov2]
    return $ Ans.Cont mov3

best :: Cache -> [Char] -> (Char -> LL Int) -> LL (Char, Int)
best cache ls m = do
  res <- forM ls $ \x -> do
    bef <- get
    case () of
      _ | x == 'S' && llRazors bef == 0 -> return (x, minf)
      _ -> do
        r <- withStep x $ do
          cur <- get
          if x `elem` "LRUD" && llPos bef == llPos cur
            then return minf
            else m x
        return (x, r)
  return $ maximumBy (comparing snd) res

search :: Field Int -> Cache -> Int -> Int -> LL (Char, Int)
search valueField cache curBest fuel
  | fuel <= 0 =
    (undefined,) <$> staticScore valueField
  | otherwise = do
    let choices= "LRUDWSA"
    st <- get
    ok <- liftIO $ addCacheEntry cache st
    if ok
      then do
      best cache choices $ \mov -> do
        mb <- score
        -- liftIO $ putStrLn $ "fuel: " ++ show fuel ++ ", mov: " ++ [mov]
        -- showStatus
        case mb of
          Nothing ->
            snd <$> search valueField cache curBest (fuel - 1)
          Just sc ->
            return sc
      else return ('A', minf)

staticScore :: Field Int -> LL Int
staticScore valueField = do
  abortScore
  {-
  aScore <- abortScore
  pos <- access llPosL
  futureScore <- unsafeReadF valueField pos
  step <- access llStepL
  return $ aScore + 0*step + futureScore
  -}
