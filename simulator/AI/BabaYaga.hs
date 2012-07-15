{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Ord
import System.IO

import Ans
import AI.Common
import DefaultMain
import LL

main :: IO ()
main = defaultMain bf

bf :: Solver IO
bf = do
  (mov, sc) <- search 6
  liftIO $ hPutStrLn stderr $ "score : " ++ show sc ++ ", move: " ++ [mov]
  return $ Ans.Cont mov

search :: (MonadIO m, Functor m) => Int -> LLT m (Char, Int)
search fuel
  | fuel <= 0 =
    (undefined,) <$> staticScore
  | otherwise = do
    best "LRUDWA" $ \mov -> withBackup $ do
      simulateStep mov
      mb <- score
      case mb of
        Nothing -> snd <$> search (fuel - 1)
        Just sc -> return sc

staticScore :: (MonadIO m, Functor m) => LLT m Int
staticScore = abortScore

best :: Monad m => [a] -> (a -> LLT m Int) -> LLT m (a, Int)
best ls m = do
  res <- forM ls $ \x -> do
    r <- m x
    return (x, r)
  return $ maximumBy (comparing snd) res
