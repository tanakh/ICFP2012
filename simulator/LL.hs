{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module LL where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Strict
import Control.Monad.Trans.Loop
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5
import Data.IORef
import Data.Lens
import Data.Lens.Template
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import System.FilePath
import Text.Printf

import qualified Ans as Ans
import qualified Option as Opt
import qualified Flood
import Pos

type Board = VM.IOVector (UM.IOVector Char)

data LLState
  = LLState
    { llStep         :: Int
    , llLambdas      :: Int
    , llTotalLambdas :: Int
    , llFlood        :: Flood.Flood
    , llPos          :: Pos
    , llBoard        :: Board
    , llHist         :: [LLState]
    , llReplay       :: [Char]
    }
nameMakeLens ''LLState $ \name -> Just (name ++ "L")

newtype LLT m a
  = LLT { unLLT :: StateT LLState m a }
  deriving ( Functor, Applicative
           , Monad, MonadIO
           , MonadState LLState, MonadTrans)

backupState :: MonadIO m => LLT m LLState
backupState = do
  bd <- access llBoardL
  nbd <- liftIO $ V.unsafeThaw =<< V.mapM UM.clone =<< V.unsafeFreeze bd
  st <- get
  return $ st { llBoard = nbd }

withBackup :: MonadIO m => LLT m a -> LLT m a
withBackup m = do
  st <- backupState
  ret <- m
  put st
  return ret

runLLT :: MonadIO m => Flood.Flood -> [String] -> LLT m a -> m a
runLLT fld bdl m = do
  let h = length bdl
      w = length $ head bdl
  bd <- liftIO $ V.thaw . V.fromList =<< mapM (U.thaw . U.fromList) bdl

  (cx, cy) <- iterateLoopT 0 $ \y -> do
    iterateLoopT 0 $ \x -> do
      when (x >= w) exit
      c <- readCell bd x y
      when (c == 'R') $ lift $ exitWith (x, y)
      return $ x+1
    return $ y+1

  lambdaNum <- liftIO $ do
    ior <- newIORef 0
    forM_ [0..h-1] $ \y -> do
      forM_ [0..w-1] $ \x -> do
        c <- readCell bd x y
        when (c == '\\') $ modifyIORef ior (+1)
    readIORef ior

  let initState = LLState
        { llStep = 1
        , llLambdas = 0
        , llTotalLambdas = lambdaNum
        , llPos = Pos cx cy
        , llBoard = bd
        , llFlood = fld
        , llHist = []
        , llReplay = []
        }
  evalStateT (unLLT m) initState

data Result
  = Win Int
  | Abort Int
  | Dead Int
  | Cont
  | Skip
  deriving (Show)

scoreResult :: Result -> Int
scoreResult (Win n) = n
scoreResult (Abort n) = n
scoreResult (Dead n) = n
scoreResult _ = assert False undefined

showStatus :: MonadIO m => LLT m ()
showStatus = do
  step <- access llStepL
  lms <- access llLambdasL
  lambdaNum <- access llTotalLambdasL
  liftIO $ printf "step: %d, lambdas: %03d/%03d, score: %d/%d/%d\n"
    step lms lambdaNum
    (lms * 75 - step)
    (lms * 50 - step)
    (lms * 25 - step)
  showBoard

showBoard :: MonadIO m => LLT m ()
showBoard = do
  bd <- access llBoardL
  (w, h) <- getSize
  liftIO $ do
    forM_ [h-1, h-2 .. 0] $ \y -> do
      forM_ [0..w-1] $ \x -> do
        putChar =<< readCell bd x y
      putStrLn ""

type Solver m = LLT m Ans.Ans

simulate :: Opt.Option -> Flood.Flood ->  [String] -> Solver IO -> IO Result
simulate opt fld bd solver = do
  runLLT fld bd $ do
    res <- once $ do
      repeatLoopT $ do
        -- pass the current status to the provider (and to player)
        when (Opt.verbose opt) $ ll showStatus
        -- receive answer
        ans <- ll $ solver
        res <- case ans of
          Ans.End -> ll $ simulateStep 'A'
          Ans.Undo -> ll undo >> continue
          Ans.Cont ch -> ll $ simulateStep ch
        case res of
          Cont -> continue
          _ -> lift $ exitWith res
      exitWith Cont

    when (Opt.verbose opt) $ do
      case res of
        Win   sc -> liftIO $ printf "You Win: %d\n" sc
        Abort sc -> liftIO $ printf "Aborted: %d\n" sc
        Dead  sc -> liftIO $ printf "You Died: %d\n" sc
        Cont     -> liftIO $ printf "Not enough input\n"
      showStatus

    rep <- access llReplayL
    when (Opt.verbose opt) $ do
      liftIO $ putStrLn $ reverse rep

    case Opt.replay opt of
      Opt.ReplayNothing -> do
        return ()
      Opt.ReplayDefault -> do
        let fn = case Opt.input opt of
              Opt.InputFile fp -> fp
              Opt.Stdin -> "STDIN"
        liftIO $ writeFile
          (printf "replay-%s-%d-%s.txt"
           (dropExtension $ takeFileName fn)
           (scoreResult res)
           (take 6 $ show $ md5 $ L.pack rep))
          rep

    return res

undo :: MonadIO m => LLT m ()
undo = do
  hist <- access llHistL
  case hist of
    [] -> do
      liftIO $ putStrLn "cannot undo"
    (top:_) -> do
      put top

simulateStep :: (Functor m, Monad m, MonadIO m) => Char -> LLT m Result
simulateStep mv = do
  step <- access llStepL
  bd <- access llBoardL
  lms <- access llLambdasL
  lambdaNum <- access llTotalLambdasL
  (w, h) <- getSize

  stat <- backupState
  llHistL %= (stat:)
  llReplayL %= (mv:)

  cont <- case mv of
    'L' -> move (-1) 0
    'R' -> move 1    0
    'U' -> move 0    1
    'D' -> move 0    (-1)
    'W' -> return Cont
    'A' -> return Cont -- abort process is below
    _   -> return Skip -- next step

  once $ do
    case cont of
      Cont -> return ()
      Skip -> continueWith Cont
      _ -> exitWith cont

    -- upadte
    nbd <- liftIO $ VM.replicateM h $ UM.replicate w ' '
    foreach [0..h-1] $ \y -> do
      foreach [0..w-1] $ \x -> do
        writeCell nbd x y =<< readCell bd x y

        c  <- readCell bd x       y
        b  <- readCell bd x       (y - 1)
        c1 <- readCell bd (x + 1) y
        b1 <- readCell bd (x + 1) (y - 1)
        c0 <- readCell bd (x - 1) y
        b0 <- readCell bd (x - 1) (y - 1)

        case () of
          _ | c == 'L' && lms == lambdaNum -> do
                writeCell nbd x y 'O'
            | c == '*' &&
              b == ' ' -> do
                writeCell nbd x (y - 1) '*'
                writeCell nbd x y ' '
            | c == '*' && c1 == ' ' &&
              b == '*' && b1 == ' ' -> do
                writeCell nbd x y ' '
                writeCell nbd (x + 1) (y - 1) '*'
            | c0 == ' ' && c == '*' &&
              b0 == ' ' && b == '*' -> do
                writeCell nbd x y ' '
                writeCell nbd (x - 1) (y - 1) '*'
            | c == '*'  && c1 == ' ' &&
              b == '\\' && b1 == ' ' -> do
                writeCell nbd x y ' '
                writeCell nbd (x + 1) (y - 1) '*'
          _ -> return ()

    lift $ llBoardL ~= nbd

    when (mv == 'A') $ do
      exitWith $ Abort $ lms * 50 - step

    Pos nx ny <- lift $ access llPosL
    a <- readCell bd  nx (ny + 1)
    b <- readCell nbd nx (ny + 1)
    when (a /= '*' && b == '*') $ do -- DEATH!!
      exitWith $ Dead $ lms * 25 - step

    lift $ llStepL += 1
    return Cont

move :: (MonadIO m, Functor m) => Int -> Int -> LLT m Result
move dx dy = do
  Pos cx cy <- access llPosL
  bd <- access llBoardL
  step <- access llStepL
  lms  <- access llLambdasL
  let (nx, ny) = (cx + dx, cy + dy)

  once $ do
    c <- readCell bd nx ny
    if c `elem` " .O\\"
      then do
      writeCell bd cx cy ' '
      writeCell bd nx ny 'R'
      when (c == 'O') $ exitWith $ Win $ lms * 75 - step
      when (c == '\\') $ lift $ void $ llLambdasL += 1
      lift $ void $ llPosL ~= Pos nx ny
      else do
      let (n2x, n2y) = (nx + dx, ny + dy)
      c2 <- readCell bd n2x n2y
      when (c2 == ' ') $ do
        writeCell bd cx cy ' '
        writeCell bd nx ny 'R'
        writeCell bd n2x n2y '*'
        lift $ void $ llPosL ~= Pos nx ny
    exitWith Cont

getSize :: (MonadIO m) => LLT m (Int, Int)
getSize = do
  bd <- access llBoardL
  let h = GM.length bd
  w <- liftIO $ GM.length <$> GM.read bd 0
  return (w, h)

whenInBound bd x y def action = liftIO $ do
  let h = GM.length bd
  w <- GM.length <$> GM.read bd 0
  if x >= 0 && x < w && y >= 0 && y < h
    then action
    else def

readCell bd x y = whenInBound bd x y (return '#') $ do
  row <- GM.read bd y
  GM.read row x

writeCell bd x y v = whenInBound bd x y (return ()) $ do
  row <- GM.read bd y
  GM.write row x v

readPos bd (Pos x y) = readCell bd x y
writePos bd (Pos x y) v = writeCell bd x y v

ll = lift . lift
