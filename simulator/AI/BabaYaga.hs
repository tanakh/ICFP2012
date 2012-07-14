{-# LANGUAGE TupleSections, ViewPatterns, RecordWildCards #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.List
import Data.Ord
import System.IO

import Ans
import AI.Common
import DefaultMain
import LL
import AI.GorinNoSho
import Data.Lens

main :: IO ()
main = defaultMain $ do
  valueField <- newF (0::Int)
  dijkstra valueField "\\O" " .*W!R" 0
  updateF valueField (75-)
  (mov, sc) <- withBackup $ search  valueField 8
  liftIO $ putStrLn $ "score : " ++ show sc ++ ", move: " ++ [mov]
  return $ Ans.Cont mov

minf :: Int
minf = -10^(9::Int)

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

search :: (MonadIO m, Functor m) => (Field Int) -> Int -> LLT m (Char, Int)
search valueField fuel
  | fuel <= 0 =
    (undefined,) <$> staticScore valueField
  | otherwise = do
    best "LRUDSA" $ \mov -> do
      mb <- score
      -- liftIO $ putStrLn $ "fuel: " ++ show fuel ++ ", mov: " ++ [mov]
      -- showStatus
      case mb of
        Nothing -> snd <$> search valueField (fuel - 1)
        Just sc -> return sc

staticScore :: (MonadIO m, Functor m) => Field Int -> LLT m Int
staticScore valueField = do
  aScore <- abortScore
  pos <- access llPosL
  futureScore <- unsafeReadF valueField pos
  step <- access llStepL
  return $ aScore + 0*step + futureScore)








