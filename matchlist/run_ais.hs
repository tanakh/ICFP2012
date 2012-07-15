{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative

import System.Exit (ExitCode (..))
import System.Process as P
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FP
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Time.Clock

import Control.Concurrent
import Control.Monad

import AIResult

data Option =
  Option { maplist :: FilePath
         , ailist  :: FilePath
         , destdir :: Maybe FilePath
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
          long "dest"
          & short 'd'
          & transform Just
          & value Nothing
          & help "dest" )
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
         -> Maybe FilePath
         -> FilePath
         -> Int
         -> Int
         -> IO ()
updateAI aipath mappath dest basename wait waitm = do
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
      result <- runAI aipath mappath dest basename wait waitm
      BL8.writeFile resultFile $ A.encode $ A.toJSON result
    resultFile = basename ++ ".result"
    rfpath = FP.decodeString resultFile

getAnswer :: String -> IO String
getAnswer ansfile = do
  lst <- lines <$> IO.readFile ansfile
  if null lst
    then return ""
    else return $ last lst

runAI :: FilePath
      -> FilePath
      -> Maybe FilePath
      -> FilePath
      -> Int
      -> Int
      -> IO SolvedResult
runAI aipath mappath dest basename wait waitm = do
  let errfn = basename ++ ".err.txt"
      outfn = basename ++ ".out.txt"
      aname = FilePath.takeBaseName aipath
      mname = FilePath.takeBaseName mappath
      aispec = AISpec aname aipath
      mapspec = MapSpec mname mappath

  mapdata <- IO.readFile mappath
  IO.withFile outfn IO.WriteMode $ \hout ->
    IO.withFile errfn IO.WriteMode $ \herr -> do
      start <- getCurrentTime
      result <- newEmptyMVar
      (_, _, _, pid) <- P.createProcess (P.proc aipath ["-i", mappath, "-t", show wait, "-v"])
        { std_out = UseHandle hout
        , std_err = UseHandle herr
        , cwd = dest
        }
      th <- forkIO $ P.waitForProcess pid >>= putMVar result . Just
      wth <- forkIO $ do
        putStrLn $ "waiting for " ++ basename ++ " " ++ show wait
        threadDelay ((wait + waitm) * 1000 * 1000)
        P.terminateProcess pid
        killThread th
        putStr $ "terminated " ++ basename
        void $ tryPutMVar result Nothing

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
      updateAI ai m (destdir opt) basename (waittime opt) (waitmore opt)
