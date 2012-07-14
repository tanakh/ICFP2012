module AI.Oracle where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5
import System.IO
import System.Cmd
import System.FilePath

import AI.Common
import State

data Oracle = 
  Oracle {
    programName :: String,
    taskName :: String,
    dbVar:: TVar (Map.Map String String),
    tejunVar :: TVar Tejun
  }

newOracle :: String -> String -> IO Oracle
newOracle prog0 task0 = do
  newDbVar <- newTVarIO Map.empty
  newTejunVar <- newTVarIO $ Tejun 0 Abort "A"
  return Oracle{programName = prog0, taskName = task0, dbVar = newDbVar, tejunVar = newTejunVar }

ask :: (Show a, Read a) => Oracle -> String -> IO a -> IO a
ask oracle key generator = do
  mx <- atomically $ do
    db <- readTVar (dbVar oracle) 
    return $ Map.lookup key db
  case mx of
    Just x -> return $ read x
    Nothing -> do
      x <- generator
      x2 <- atomically $ do
          db <- readTVar (dbVar oracle) 
          case Map.lookup key db of
            Just x1 -> return $ read x1
            Nothing -> do
              modifyTVar (dbVar oracle) (Map.insert key (show x))
              return x
      return x2

submit :: Oracle -> Tejun -> IO ()
submit oracle tejun = do
  atomically $ modifyTVar (tejunVar oracle) $ max tejun

save :: Oracle -> IO ()
save oracle = do
  db <- atomically $ readTVar (dbVar oracle)
  Tejun score res rep <- atomically $ readTVar (tejunVar oracle)
  let dirName = printf "oracle/%s/" (dropExtension $ takeFileName $ taskName oracle)
      fileName = printf "%s/%d-%s.txt" score (take 6 $ show $ md5 $ L.pack dbstr)
      dbstr = show db
  system $ "mkdir -p " ++ dirName
  writeFile fileName $ unlines
    [show score,
     show res,
     rep,
     dbstr
    ]