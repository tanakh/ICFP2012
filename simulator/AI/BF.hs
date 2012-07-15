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

goodnessCheck :: History -> History -> Int ->  Char -> LL Int
goodnessCheck hist hyperhist fuel hand
  | fuel <= 0 = withStep hand $ do
                       res <- access llResultL
                       h <- access llHashL
                       step <- access llStepL
                       cnt <- liftIO $ readHistory hist h
                       cnt2 <- liftIO $ readHistory hyperhist h
                       return $ case () of
                                  _ | res == Dead        -> -3
                                    | cnt > 0            -> -2
                                    | cnt2*step >  0     -> -1
                                    | True               ->  0
  | otherwise = withStep hand $ do
                    gs <- (forM "LRUD" $ goodnessCheck hist hyperhist (fuel-1))
                    return $ maximum gs
main :: IO ()
main = do
  opt <- Option.parseIO
  valueFieldRef <- newIORef undefined
  loveFieldRef <- newIORef undefined
  let inputfn = case Option.input opt of
            Option.InputFile fp -> fp
            Option.Stdin -> "STDIN"
  oracle <- Oracle.new inputfn
  when (Option.oracleSource opt/= "") $ do
    Oracle.load oracle $ Option.oracleSource opt
  hyperHistory <- newIORef HM.empty  

  infiniteLoop <- liftIO $ Oracle.ask oracle "infiniteLoop" $ return False
  (if infiniteLoop then forever else id) $ do
    -- generate randomize parameters
    motionWeight <- forM "LRUD" $ \char -> do
      w <- randomRIO (0.1, 3)
      return (char, w)
    history <- newIORef HM.empty

    earthDrugAmp <- liftIO $ Oracle.ask oracle "earthDrugAmp" $ return (0.0::Double)
    earthDrug <- exp <$> randomRIO (- earthDrugAmp, earthDrugAmp) 
    itemLoveAmp <- liftIO $ Oracle.ask oracle "itemLoveAmp" $ return (0.0:: Double)
    placeLoveAmp <- liftIO $ Oracle.ask oracle "placeLoveAmp" $ return (0.0:: Double)
    placeLoveNum <- liftIO $ Oracle.ask oracle "placeLoveNum" $ return (0.0:: Double)
    

    --- start of One Challenge
    defaultMain oracle $ do
      step <- access llStepL
      (liftIO . Oracle.submit oracle) =<< getAbortTejun
  
      bfDepth <- liftIO $ Oracle.ask oracle "bfDepth" $ return 10
      hmr <- liftIO $ newIORef HM.empty

      hashNow <- access llHashL
      kabutta <- (>2) <$> (liftIO $ readHistory history hashNow)
      liftIO $ modifyHistory history hashNow (1+)  

      liftIO $ do
        useHyperHistory <- Oracle.ask oracle "useHyperHistory" $ return False
        when (useHyperHistory) $ modifyHistory hyperHistory hashNow (1+)  
  
      (mov, sc) <- withBackup $ do
        rs <- forM moves $ \mov -> withStep mov $ do
          -- TODO: unify
          (mov, ) <$> eval undefined hmr 0 bfDepth
        return $ maximumBy (comparing snd) rs
  
      (w,h) <- getSize
      roboPos <- access llPosL
      let radius :: Double
          radius = fromIntegral $ w+h
      bd <- access llBoardL

      greedyDepth <- liftIO $ Oracle.ask oracle "greedyDepth" $ return 4
      valueField <- 
        if step > 0 
          then liftIO $ readIORef valueFieldRef
          else do
            ret <- newF (0::Double)
            liftIO $ writeIORef valueFieldRef ret
            return ret
      loveField <- 
        if step > 0 
          then liftIO $ readIORef loveFieldRef
          else do
            newLoveField <- newF (0::Double)
            forPos $ \ r -> do
              a <- unsafeReadF bd r
              when (a `elem` "ABCDEFGHI!" && itemLoveAmp > 0) $ 
                writeF newLoveField r =<< liftIO (randomRIO (0, itemLoveAmp * radius))
              when (placeLoveNum > 0) $ do
                dice <- liftIO $ randomRIO (0,1)
                when (dice < placeLoveNum /(fromIntegral $ w*h)) $ do
                  writeF newLoveField r =<< liftIO (randomRIO (0, placeLoveAmp * radius))                
            liftIO $ writeIORef loveFieldRef newLoveField
            return newLoveField
      -- love disappears when you reach there...
      writeF loveField roboPos 0

      razors <- access llRazorsL             
      let passable 
            | razors > 0 = " .!\\RWA"
            | otherwise  = " .!\\RA"
      dijkstraEX valueField "\\O" passable motionWeight earthDrug loveField 0
      updateF valueField (\x -> max 0 $ 75-x)
      val <- unsafeReadF valueField roboPos
      (mov2, confidence) <-  do
               cand <- forM "LRUD" $ \hand -> do
                     val3 <- unsafeReadF valueField $ roboPos + hand2pos hand
                     flag <- goodnessCheck history hyperHistory greedyDepth hand
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
