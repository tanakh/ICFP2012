{-# LANGUAGE TupleSections, ViewPatterns, RecordWildCards #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List
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

safetyCheck ::(Functor m,MonadIO m)=> Char -> LLT m Bool
safetyCheck hand = withStep hand $ do
  res <- access llResultL
  return (res /= Dead)

main :: IO ()
main = do
  opt <- Option.parseIO
  let inputfn = case Option.input opt of
            Option.InputFile fp -> fp
            Option.Stdin -> "STDIN"
  oracle <- Oracle.new inputfn
  defaultMain oracle $ do
    valueField <- newF (0::Int)
    dijkstra valueField "\\O" " .!\\R" 0
    updateF valueField (\x -> max 0 $ 75-x)
    hmr <- liftIO $ newIORef HM.empty
    (mov, sc) <- withBackup $ search valueField hmr 10
    
    (liftIO . Oracle.submit oracle) =<< getAbortTejun

    roboPos <- access llPosL
    val <- unsafeReadF valueField roboPos
    mov2 <- if mov /= 'A' || val <= 0 
               then return mov
               else do
                   cand <- forM "URLD" $ \hand -> do
                         val3 <- unsafeReadF valueField $ roboPos + hand2pos hand
                         flag <- safetyCheck hand
                         return ((flag,val3), hand)
                   return $ snd $ last $ sort $ cand
    liftIO $ putStrLn $ "score : " ++ show sc ++ ", move: " ++ nub [mov,mov2]
    return $ Ans.Cont mov2

best :: (Functor m, MonadIO m)
        => [Char] -> (Char -> LLT m Int) -> LLT m (Char, Int)
best ls m = do
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

search :: (MonadIO m, Functor m)
          => Field Int -> Cache -> Int -> LLT m (Char, Int)
search valueField cache fuel
  | fuel <= 0 =
    (undefined,) <$> staticScore valueField
  | otherwise = do
    let choices= "LRUDWSA"
    st <- get
    ok <- liftIO $ addCacheEntry cache st
    if ok
      then do
      best choices $ \mov -> do
        mb <- score
        -- liftIO $ putStrLn $ "fuel: " ++ show fuel ++ ", mov: " ++ [mov]
        -- showStatus
        case mb of
          Nothing -> snd <$> search valueField cache (fuel - 1)
          Just sc -> return sc
      else do
      return ('W', minf)

staticScore :: (MonadIO m, Functor m) => Field Int -> LLT m Int
staticScore valueField = do
  abortScore
  {-
  aScore <- abortScore
  pos <- access llPosL
  futureScore <- unsafeReadF valueField pos
  step <- access llStepL
  return $ aScore + 0*step + futureScore
  -}
