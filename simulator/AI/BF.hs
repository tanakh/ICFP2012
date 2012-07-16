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
import AI.Cooking(choose)
import Pos

minf :: Int
minf = -10^(8::Int)

moves :: [Char]
moves = "LRUDWSA"

type LL = LLT IO

type Cache = IORef (HM.HashMap Word64 [CacheEntry])

data CacheEntry
  = CacheEntry
    { ceStep      :: {-# UNPACK #-} !Int
    , ceWaterStep :: {-# UNPACK #-} !Int
    , ceRazors    :: {-# UNPACK #-} !Int
    , cePenalty   :: {-# UNPACK #-} !Int
    , ceScore     :: {-# UNPACK #-} !Int
    }

isWorseThan :: CacheEntry -> CacheEntry -> Bool
a `isWorseThan` b =
  ceStep a >= ceStep b
  && ceWaterStep a >= ceWaterStep b
  && ceRazors a <= ceRazors b
  && (ceScore a, 0) <= (ceScore b, 0)
  --  && (ceScore a, - cePenalty a) <= (ceScore b, - cePenalty b)

addCacheEntry :: IORef (HM.HashMap Word64 [CacheEntry]) -> LL Int -> LL Int
addCacheEntry hmr m = do
  st <- get

  hm <- liftIO $ readIORef hmr
  let ce = CacheEntry
             { ceStep = llStep st
             , ceWaterStep = llWaterStep st
             , ceRazors = llRazors st
             , cePenalty = llPenalty st
             , ceScore = llLambdas st
             }
  let cm = case HM.lookup (llHash st) hm of
        Just cfs
          | not $ any (ce `isWorseThan`) cfs -> Nothing
          | otherwise -> Just $ maximum $ map ceScore cfs
        _ -> Nothing

  case cm of
    Nothing -> do
      liftIO $ modifyIORef hmr $ HM.insertWith (++) (llHash st) [ce]
      sc <- m
      --liftIO $ modifyIORef hmr $ HM.insertWith (++) (llHash st)
      --  [ce { ceScore = sc }]
      return sc
    Just _ ->
      return minf

safetyCheck :: HS.HashSet Word64 -> Int ->  Char -> LL Bool
safetyCheck hist fuel hand
  | fuel <= 0 = withStep hand $ do
                       res <- access llResultL
                       h <- access llHashL
                       return $ res /= Dead && not (HS.member h hist)
  | otherwise = withStep hand $
                     or <$> (forM "LRUD" $ safetyCheck hist (fuel-1))

adjR =
  [ Pos 1 0
  , Pos 0 1
  , Pos (-1) 0
  ]

simplify :: LL ()
simplify = do
  bd <- access llBoardL
  forPos $ \p -> do
    cell <- liftIO $ readPos bd p
    liftIO $ when (cell == '.') $ do
      keep <- forM adjR $ \((+p) -> n) -> do
        nell <- readPos bd n
        return $ isRock nell || nell == 'W'
      when (not $ or keep) $ do
        writePos bd p ' '
  liftIO $ putStrLn "simplify to:"
  showBoard

main :: IO ()
main = do
  opt <- Option.parseIO
  historyRef <- newIORef HS.empty
  valueFieldRef <- newIORef undefined
  hashLogRef <- newIORef []
  let inputfn = case Option.input opt of
            Option.InputFile fp -> fp
            Option.Stdin -> "STDIN"
  oracle <- Oracle.new inputfn
  defaultMain oracle $ do
    step <- access llStepL
    (liftIO . Oracle.submit oracle) =<< getAbortTejun

    bfDepth <- liftIO $ Oracle.ask oracle "bfDepth" $ return 20
    hmr <- liftIO $ newIORef HM.empty
    hashNow <- access llHashL

    kabutta <- liftIO $ do
           modifyIORef hashLogRef (hashNow:)
           xs <- readIORef hashLogRef
           return $
             (length (take 3 xs) >= 3)
             && ((1==) $length $ nub $take 3 xs)
    liftIO $ modifyIORef historyRef $ HS.insert hashNow
    history <- liftIO $ readIORef historyRef


    (mov, sc) <- withBackup $ do
      simplify
      rs <- forM moves $ \mov -> do
        cur <- get
        b <- prePruning cur mov
        if b
          then do
          withStep mov $ do
            next <- get
            c <- pruning cur mov next
            if c
              then (mov, ) <$> eval undefined hmr 0 bfDepth
              else return (mov, minf)
          else return (mov, minf)
      liftIO $ print rs
      liftIO $ print . HM.size =<< readIORef hmr
      liftIO $ print . sum . map length . map snd . HM.toList =<< readIORef hmr
      return $ maximumBy (comparing snd) rs

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
    perfectGreedy <- liftIO $ Oracle.ask oracle "perfectGreedy" $ return False
    movRand <- liftIO $ choose "RLDU"
    let mov3
         | kabutta                                    = movRand
         | perfectGreedy                              = mov2
         | combineBFFirst && (mov /= 'A' || val <= 0) = mov
         | combineBFFirst                             = mov2
         | not (combineBFFirst) && confidence         = mov2
         | otherwise                                  = mov

    when (Option.verbose opt) $
      liftIO $ putStrLn $ "score : " ++ show sc ++ ", move: " ++  [mov,mov2]
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

scoreOfs = 10^5

staticScore :: Field Int -> LL Int
staticScore _ = do
  LLState {..} <- get
  abt <- abortScore
  return $ (abt * scoreOfs) - llPenalty
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
    Just sc -> return $ sc * scoreOfs
    _ | fuel <= 0 -> staticScore valueField
    _ -> do
      addCacheEntry cache $ do
        best cache moves $ \mov -> do
          -- liftIO $ putStrLn $ "fuel: " ++ show fuel ++ ", mov: " ++ [mov]
          -- showStatus
          eval valueField cache curBest (fuel - 1)

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
