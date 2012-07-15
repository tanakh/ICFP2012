{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module AIResult where

import Control.Concurrent
import Control.Monad

import Data.Aeson.TH
import Data.Data
import Provider
import qualified Ans
import LL (simulate, isValidInput)

data AISpec =
  AISpec
  { aiName :: String
  , aiPath :: FilePath
  }
  deriving (Show, Data, Typeable, Ord, Eq)
$(deriveJSON (drop 2) ''AISpec)

data MapSpec =
  MapSpec
  { mapName :: String
  , mapPath :: FilePath
  }
  deriving (Show, Data, Typeable, Ord, Eq)
$(deriveJSON (drop 3) ''MapSpec)

data Score =
  Score
  { scoreResult :: String
  , scoreValue :: Int
  }
  deriving (Show, Data, Typeable)
$(deriveJSON (drop 4) ''Score)

data SolvedResult =
  SolvedResult
  { sExitCode :: Maybe Int
  , sMapSpec :: MapSpec
  , sAISpec :: AISpec
  , sBaseName :: String
  , sScore :: Score
  , sTimeElapsed :: Double
  }
  deriving (Show, Data, Typeable)
$(deriveJSON (drop 1) ''SolvedResult)

stringProvider :: String -> Provider
stringProvider ans mvAns mvState = do
  void $ forkIO $ blackhole mvState
  mapM_ (putMVar mvAns) $ map Ans.Cont $ filter isValidInput ans
  putMVar mvAns Ans.End

calcScore :: String -> String -> IO Score
calcScore mapdata ans = do
  ansMVar <- newEmptyMVar
  stateMVar <- newEmptyMVar
  solver <- do
    void $ forkIO $ stringProvider ans ansMVar stateMVar
    return $ providedSolver ansMVar stateMVar

  let verbose = False
  (res, score, _) <- simulate verbose mapdata solver

  return $ Score (show res) score
