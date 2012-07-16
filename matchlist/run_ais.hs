{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative

import System.Exit (ExitCode (..))
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FP
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified GHC.IO.Handle as H
import qualified System.Posix.Signals as Signal
import qualified System.Posix.Process as P
import System.Posix.Types (ProcessID)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Time.Clock

import Control.Concurrent
import Control.Monad

import AIResult

data Option =
  Option { maplist :: FilePath
         , ailist  :: FilePath
         , waittime :: Int
         , waitmore :: Int
         }

parseIO :: IO Option
parseIO = execParser $ info (helper <*> parse)
          ( fullDesc
            & header "*** Simulator Runner ***"
            & progDesc (unlines [])
          )

parse :: Parser Option
parse = Option
        <$> strOption (
          long "map"
          & short 'm'
          & value "maplist"
          & help "maplists" )
        <*> strOption (
          long "ai"
          & short 'a'
          & value "ailist"
          & help "ailist" )
        <*> strOption (
          long "wait"
          & short 'w'
          & transform read
          & value 150
          & help "waittime" )
        <*> strOption (
          long "waitmore"
          & short 'm'
          & transform read
          & value 10
          & help "kill if (wait + waitmore) exceeds" )

updateAI :: FilePath
         -> FilePath
         -> FilePath
         -> Int
         -> Int
         -> IO ()
updateAI aipath mappath basename wait waitm = do
  exist <- FS.isFile rfpath
  if exist
    then do
      am <- FS.getModified . FP.decodeString $ aipath
      mm <- FS.getModified . FP.decodeString $ mappath
      lastm <- FS.getModified rfpath
      when (lastm < am || lastm < mm) run
    else run
  where
    run = do
      putStrLn basename
      result <- runAI aipath mappath basename wait waitm
      BL8.writeFile resultFile $ A.encode $ A.toJSON result
    resultFile = basename ++ ".result"
    rfpath = FP.decodeString resultFile

getAnswer :: String -> IO String
getAnswer ansfile = do
  lst <- lines <$> IO.readFile ansfile
  if null lst
    then return ""
    else return $ last lst

createProcess
  :: FilePath -> [String] -> H.Handle -> H.Handle
  -> IO ProcessID
createProcess cmd args stdo stde = P.forkProcess $ do
  void $ H.hDuplicate IO.stdout
  H.hDuplicateTo stdo IO.stdout
  H.hClose stdo
  void $ H.hDuplicate IO.stderr
  H.hDuplicateTo stde IO.stderr
  H.hClose stde
  P.executeFile cmd True args Nothing

runAI :: FilePath
      -> FilePath
      -> FilePath
      -> Int
      -> Int
      -> IO SolvedResult
runAI aipath mappath basename wait waitm = do
  let errfn = basename ++ ".err.txt"
      outfn = basename ++ ".out.txt"
      aname = FilePath.takeBaseName aipath
      mname = FilePath.takeBaseName mappath
      aispec = AISpec aname aipath
      mapspec = MapSpec mname mappath

  mapdata <- IO.readFile mappath
  result <- newEmptyMVar
  (start, _, _, wth) <- IO.withFile outfn IO.WriteMode $ \hout ->
    IO.withFile errfn IO.WriteMode $ \herr -> do
      start <- getCurrentTime

      pid <- createProcess aipath ["-i", mappath, "-t", show wait, "-v"] hout herr
      th <- forkIO $ do
        mstat <- P.getProcessStatus True False pid
        let st = case mstat of
              Just (P.Exited ec) -> Just ec
              _ -> Nothing
        void $ tryPutMVar result st

      wth <- forkIO $ do
        putStrLn $ "waiting for " ++ basename ++ " " ++ show wait
        threadDelay (wait * 1000 * 1000)
        putStrLn $ ("will send SIGINT to " ++ basename ++ ", and waiting " ++ show waitm)
        Signal.signalProcess Signal.sigINT pid
        threadDelay (waitm * 1000 * 1000)
        putStrLn $ "send SIGTERM: " ++ basename
        Signal.signalProcess Signal.sigTERM pid
        killThread th
        putStrLn $ "terminated " ++ basename
        void $ tryPutMVar result Nothing

      return (start, pid, th, wth)
  ret <- takeMVar result
  elapsed <- (`diffUTCTime` start) <$> getCurrentTime
  let es = fromRational . toRational $ elapsed
  killThread wth

  ans <- getAnswer outfn
  score <- calcScore mapdata ans

  return $ SolvedResult { sExitCode = fromExitCode <$> ret
                        , sMapSpec = mapspec
                        , sAISpec = aispec
                        , sBaseName = basename
                        , sScore = score
                        , sTimeElapsed = es
                        }

  where
    fromExitCode ExitSuccess = 0
    fromExitCode (ExitFailure i) = i

main :: IO ()
main = do
  opt <- parseIO
  maps <- lines <$> (readFile $ maplist opt)
  ais <- lines <$> (readFile $ ailist opt)
  forM_ maps $ \m -> do
    let mapname = FilePath.takeBaseName m
    forM_ ais $ \ai -> do
      let ainame = FilePath.takeBaseName ai
          basename = ainame ++ "_" ++ mapname
      updateAI ai m basename (waittime opt) (waitmore opt)
