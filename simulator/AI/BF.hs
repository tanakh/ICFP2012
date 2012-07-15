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

minf :: Int
minf = -10^(9::Int)

main :: IO ()
main = defaultMain $ do
  (mov, sc) <- withBackup $ search 10
  liftIO $ putStrLn $ "score : " ++ show sc ++ ", move: " ++ [mov]
  return $ Ans.Cont mov

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

search :: (MonadIO m, Functor m) => Int -> LLT m (Char, Int)
search fuel
  | fuel <= 0 =
    (undefined,) <$> staticScore
  | otherwise = do
    best "LRUDSA" $ \mov -> do
      mb <- score
      -- liftIO $ putStrLn $ "fuel: " ++ show fuel ++ ", mov: " ++ [mov]
      -- showStatus
      case mb of
        Nothing -> snd <$> search (fuel - 1)
        Just sc -> return sc

staticScore :: (MonadIO m, Functor m) => LLT m Int
staticScore = abortScore
