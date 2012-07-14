{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module LL where

import Control.Applicative
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
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import System.FilePath
import System.IO
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
    , llLiftPos      :: Pos
    , llRockPos      :: VM.IOVector Pos
    , llBoard        :: Board
    , llWaterStep    :: Int
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
  rocks <- access llRockPosL
  nrocks <- liftIO $ VM.clone rocks
  st <- get
  return $ st { llBoard = nbd, llRockPos = nrocks}

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

  (cxLift, cyLift) <- iterateLoopT 0 $ \y -> do
    iterateLoopT 0 $ \x -> do
      when (x >= w) exit
      c <- readCell bd x y
      when (c == 'L') $ lift $ exitWith (x, y)
      return $ x+1
    return $ y+1

  lambdaNum <- liftIO $ do
    ior <- newIORef 0
    forM_ [0..h-1] $ \y -> do
      forM_ [0..w-1] $ \x -> do
        c <- readCell bd x y
        when (c == '\\') $ modifyIORef ior (+1)
    readIORef ior

  rocks <- liftIO $ do
    rr <- newIORef []
    forM_ [0..h-1] $ \y -> do
      forM_ [0..w-1] $ \x -> do
        c <- readCell bd x y
        when (c == '*') $ modifyIORef rr (Pos x y:)
    reverse <$> readIORef rr
  vrocks <- liftIO $ V.thaw $ V.fromList rocks

  let initState = LLState
        { llStep = 0
        , llLambdas = 0
        , llTotalLambdas = lambdaNum
        , llPos = Pos cx cy
        , llLiftPos = Pos cxLift cyLift
        , llRockPos = vrocks
        , llBoard = bd
        , llFlood = fld
        , llWaterStep = 0
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

winScore, abortScore, deathScore :: MonadIO m => LLT m Int
winScore = do
  step <- access llStepL
  lms <- access llLambdasL
  return  (lms * 75 - step)

abortScore = do
  step <- access llStepL
  lms <- access llLambdasL
  return (lms * 50 - step)

deathScore = do
  step <- access llStepL
  lms <- access llLambdasL
  return (lms * 25 - step)

showStatus :: MonadIO m => LLT m ()
showStatus = do
  step <- access llStepL
  lms <- access llLambdasL
  lambdaNum <- access llTotalLambdasL
  score1 <- winScore
  score2 <- abortScore
  score3 <- deathScore
  liftIO $ printf "step: %d, lambdas: %03d/%03d, score: %d/%d/%d\n"
    step lms lambdaNum
    score1 score2 score3

  fld <- access llFloodL
  ws <- access llWaterStepL
  liftIO $ printf "water: %02d/%02d\n"
    ws (Flood.waterproof fld)
  showBoard

showBoard :: MonadIO m => LLT m ()
showBoard = do
  bd <- access llBoardL
  step <- access llStepL
  fld <- access llFloodL
  let wl = Flood.waterLevel step fld
  (w, h) <- getSize
  liftIO $ do
    forM_ [h-1, h-2 .. 0] $ \y -> do
      forM_ [0..w-1] $ \x -> do
        hPutChar stderr =<< readCell bd x y
      hPutStrLn stderr $ if y < wl then "~~~~" else "    "

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

    rep <- reverse <$> access llReplayL
    when (Opt.verbose opt) $ do
      liftIO $ putStrLn rep

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

    when (Opt.input opt == Opt.Stdin) $ do
      liftIO $ putStrLn rep
      liftIO $ hFlush stdout
    return res

undo :: MonadIO m => LLT m ()
undo = do
  hist <- access llHistL
  case hist of
    [] -> do
      liftIO $ putStrLn "cannot undo"
    (top:_) -> do
      put top

simulateStep :: (Functor m, MonadIO m) => Char -> LLT m Result
simulateStep mv = do
  fld  <- access llFloodL
  step <- access llStepL
  bd   <- access llBoardL
  lambdaNum <- access llTotalLambdasL
  (w, h) <- getSize

  stat <- backupState
  llHistL %= (stat:)
  llReplayL %= (mv:)

  cont <- moveC mv
  lms <- access llLambdasL

  once $ do
    case cont of
      Cont -> return ()
      Skip -> continueWith Cont -- is it bug?
      _ -> exitWith cont

    cp@(Pos nx ny) <- lift $ access llPosL
    bup <- readPos bd $ cp + Pos 0 1

    -- update

    when (lms == lambdaNum) $ do
      lpos <- lift $ access llLiftPosL
      lc <- readPos bd lpos
      when (lc == 'L') $ do
        writePos bd lpos 'O'

    rocks <- lift $ access llRockPosL
    forM_ [0 .. GM.length rocks - 1] $ \ix -> do
      p <- liftIO $ GM.read rocks ix

      c  <- readPos bd $ p + Pos 0 0
      b  <- readPos bd $ p + Pos 0 (-1)
      c1 <- readPos bd $ p + Pos 1 0
      b1 <- readPos bd $ p + Pos 1 (-1)
      c0 <- readPos bd $ p + Pos (-1) 0
      b0 <- readPos bd $ p + Pos (-1) (-1)

      case () of
        _ | c == '*' &&
            b == ' ' -> do
              writePos bd p ' '
              writePos bd (p + Pos 0 (-1)) '*'
              liftIO $ GM.write rocks ix $ p + Pos 0 (-1)
          | c == '*' && c1 == ' ' &&
            b == '*' && b1 == ' ' -> do
              writePos bd p ' '
              writePos bd (p + Pos 1 (-1)) '*'
              liftIO $ GM.write rocks ix $ p + Pos 1 (-1)
          | c0 == ' ' && c == '*' &&
            b0 == ' ' && b == '*' -> do
              writePos bd p ' '
              writePos bd (p + Pos (-1) (-1)) '*'
              liftIO $ GM.write rocks ix $ p + Pos (-1) (-1)
          | c == '*'  && c1 == ' ' &&
            b == '\\' && b1 == ' ' -> do
              writePos bd p ' '
              writePos bd (p + Pos 1 (-1)) '*'
              liftIO $ GM.write rocks ix $ p + Pos 1 (-1)
        _ -> return ()

    -- invariant
    liftIO $ Intro.sortBy (comparing $ \(Pos x y) -> (y, x)) rocks

    when (mv == 'A') $ do
      exitWith $ Abort $ lms * 50 - step

    lift $ llStepL += 1  -- increment step if it is not Abort

    cup <- readPos bd $ cp + Pos 0 1
    when (bup /= '*' && cup == '*') $ do -- DEATH!!
      exitWith $ Dead $ lms * 25 - (step + 1)

    let wl = Flood.waterLevel step fld
    ws0 <- lift $ access llWaterStepL
    let ws = if ny < wl  -- in the water
                then ws0 + 1
                else 0
    lift $ llWaterStepL ~= ws
    when (ws > Flood.waterproof fld) $ do
      exitWith $ Dead $ lms * 25 - (step + 1)

    return Cont

move :: (MonadIO m, Functor m) => Int -> Int -> LLT m Result
move dx dy = do
  p0 <- access llPosL
  bd <- access llBoardL
  step <- access llStepL
  lms  <- access llLambdasL
  let d = Pos dx dy
      p1 = p0 + d
      p2 = p0 + d + d
  c0 <- readPos bd p0
  c1 <- readPos bd p1
  c2 <- readPos bd p2

  case () of
    _ | c1 `elem` " .O\\" -> do
        writePos bd p0 ' '
        writePos bd p1 'R'
        when (c1 == '\\') $ void $ llLambdasL += 1
        llPosL ~= p1
        if c1 == 'O'
          then do
            llStepL += 1
            Win <$> winScore
          else return Cont
      | dy == 0 && c0 == 'R' && c1 == '*' && c2 == ' ' -> do
        writePos bd p0 ' '
        writePos bd p1 'R'
        writePos bd p2 '*'
        llPosL ~= p1
        rocks <- access llRockPosL
        forM_ [0..GM.length rocks - 1] $ \ix -> do
          p <- liftIO $ GM.read rocks ix
          -- rock moves p1 => p2
          liftIO $ when (p == p1) $ GM.write rocks ix p2
        return Cont
    _ ->
      return Cont

moveC :: (MonadIO m, Functor m) => Char -> LLT m Result
moveC c = case c of
  'L' -> move (-1) 0
  'R' -> move 1    0
  'U' -> move 0    1
  'D' -> move 0    (-1)
  'W' -> return Cont
  'A' -> return Cont -- abort process is below
  _   -> return Skip -- next step

getSize :: (MonadIO m) => LLT m (Int, Int)
getSize = do
  bd <- access llBoardL
  let h = GM.length bd
  w <- liftIO $ GM.length <$> GM.read bd 0
  return (w, h)

forPos :: MonadIO m => (Pos -> LLT m ()) -> LLT m ()
forPos m = do
  (w, h) <- getSize
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      m (Pos x y)

loopPos :: MonadIO m => (Pos -> LoopT c () (LLT m) c) -> LLT m ()
loopPos m = do
  (w, h) <- getSize
  foreach [ Pos x y | y <- [0..h-1], x <- [0..w-1] ] $ \pos -> do
    m pos

whenInBound bd x y def action = liftIO $ do
  let h = GM.length bd
  w <- GM.length <$> GM.read bd 0
  if x >= 0 && x < w && y >= 0 && y < h
    then action
    else def

readCell bd x y = whenInBound bd x y (return '#') $ do
  row <- GM.read bd y
  GM.read row x

readCellMaybe bd x y = whenInBound bd x y (return Nothing) $ do
  row <- GM.read bd y
  Just <$> GM.read row x

readCellList bd x y = whenInBound bd x y (return []) $ do
  row <- GM.read bd y
  (:[]) <$> GM.read row x

writeCell bd x y v = whenInBound bd x y (return ()) $ do
  row <- GM.read bd y
  GM.write row x v

readPos bd (Pos x y) = readCell bd x y
writePos bd (Pos x y) v = writeCell bd x y v

readPosMaybe bd (Pos x y) = readCellMaybe bd x y
readPosList bd (Pos x y) = readCellList bd x y

ll = lift . lift
