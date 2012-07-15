{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module LL (
  -- simulate whole game
  simulate,

  -- the LLT monad
  LLT, runLLT, Solver,

  -- exec step, and undo
  simulateStep, undo,
  isEnd, score, abortScore, deadScore, winScore,

  -- aux
  getSize,
  forPos, loopPos,
  whenInBoundPos,
  readPos, readPosM, writePos,
  getReplay,

  -- full backup, and restore (maybe heave...)
  backupState, restoreState, withBackup,

  -- diagnostic
  showStatus, showBoard,

  module State,
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Strict
import Control.Monad.Trans.Loop
import Data.IORef
import Data.Lens
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import System.IO
import Text.Printf

import qualified Ans
import qualified Flood
import Pos
import State

newtype LLT m a
  = LLT { unLLT :: StateT LLState m a }
  deriving ( Functor, Applicative
           , Monad, MonadIO
           , MonadState LLState, MonadTrans)

type Solver m = LLT m Ans.Ans

isEnd :: Monad m => LLT m Bool
isEnd = do
  res <- access llResultL
  return $ res /= Cont

score :: MonadIO m => LLT m (Maybe Int)
score = do
  res <- access llResultL
  case res of
    Win   -> return . Just =<< winScore
    Abort -> return . Just =<< abortScore
    Dead  -> return . Just =<< deadScore
    Cont  -> return Nothing

winScore, abortScore, deadScore :: MonadIO m => LLT m Int
winScore = do
  step <- access llStepL
  lms <- access llLambdasL
  return  (lms * 75 - step)

abortScore = do
  step <- access llStepL
  lms <- access llLambdasL
  return (lms * 50 - step)

deadScore = do
  step <- access llStepL
  lms <- access llLambdasL
  return (lms * 25 - step)

-----

runLLT :: MonadIO m => String -> LLT m a -> m a
runLLT txt m = do
  let (txtB,txtM) = span (/="") $ lines txt
      bd0 = reverse txtB
      w = maximum $ map length bd0
      bdl = map (take w . (++ repeat ' ')) bd0

  let finds p =
        [ (cell, Pos (x :: Int) (y ::Int))
        | (y, row)  <- zip [0..] bdl, (x, cell) <- zip [0..] row, p cell
        ]

  let fld = Flood.readFlood $ drop 1 txtM

  let tramps  = finds (`elem` ['A' .. 'I'])
      targets = finds (`elem` ['1' .. '9'])
      trampp =
        [ (f, t) | ["Trampoline", [f], "targets", [t]] <- map words txtM ]
      tramp =
        [ (f, (t, tp, map (\e -> fromJust $ lookup e tramps) erases))
        | (f, t) <- trampp
        , let Just tp = lookup t targets
              erases = map fst $ filter ((==t).snd) trampp
        ]

  let rpos      = snd . head $ finds (=='R')
      lpos      = snd . head $ finds (=='L')
      lambdaNum = length $ finds (=='\\')
      rocks     = map snd $ finds (=='*')

  bd <- liftIO $ V.thaw . V.fromList =<< mapM (U.thaw . U.fromList) bdl

  let initState = LLState
        { llTotalLambdas = lambdaNum
        , llLiftPos = lpos
        , llFlood = fld
        , llTramp = tramp

        , llResult = Cont
        , llStep = 0
        , llPos = rpos
        , llLambdas = 0

        , llRockPos = sort rocks
        , llWaterStep = 0

        , llBoard = bd

        , llPatches = []
        }
  evalStateT (unLLT m) initState

showStatus :: MonadIO m => LLT m ()
showStatus = do
  LLState {..} <- get
  score1 <- winScore
  score2 <- abortScore
  score3 <- deadScore

  liftIO $ do
    printf "step: %d, lambdas: %03d/%03d, score: %d/%d/%d\n"
      llStep llLambdas llTotalLambdas
      score1 score2 score3

    printf "water: %02d/%02d\n"
      llWaterStep (Flood.waterproof llFlood)

    when (not $ null llTramp) $ do
      putStrLn $ "trampoline: " ++
        ( intercalate ", "
          $ map (\(f, (t, _, _)) -> [f] ++ "->" ++ [t]) llTramp )

  showBoard

showBoard :: MonadIO m => LLT m ()
showBoard = do
  LLState {..} <- get
  (w, h) <- getSize
  let wl = Flood.waterLevel llStep llFlood
  liftIO $ do
    forM_ [h-1, h-2 .. 0] $ \y -> do
      hPutStr stderr =<< forM [0..w-1] (\x -> readPos llBoard $ Pos x y)
      hPutStrLn stderr $ if y < wl then "~~~~" else "    "

getReplay :: (Functor m, MonadIO m) => LLT m String
getReplay = reverse . map pMove <$> access llPatchesL

simulate :: Bool -> String -> Solver IO
            -> IO (Result, Int, String) -- (result, score, replay)
simulate interactive txt solver = runLLT txt go where
  go = do
    -- pass the current status to the provider (and to player)
    when interactive $ showStatus

    -- get move from solver and simulate one step
    mv <- solver
    case mv of
      Ans.Undo -> undo -- assert interactive undo
      Ans.End     -> do
        when (not interactive) $ liftIO $ putChar 'A' >> hFlush stdout
        simulateStep 'A'
      Ans.Cont ch -> do
        when (not interactive) $ liftIO $ putChar ch >> hFlush stdout
        simulateStep ch

    mb <- score
    case mb of
      Nothing -> go
      Just sc -> do
        res     <- access llResultL
        replay  <- getReplay
        when interactive $ showStatus
        return (res, sc, replay)

-- simualte and undo

simulateStep :: (Functor m, MonadIO m) => Char -> LLT m ()
simulateStep mv
  | mv == 'A' = do -- abort process immediately
    stash 'A'
    void $ llResultL ~= Abort
  | otherwise = do
    stash mv
    wlog <- liftIO $ newIORef []
    rlog <- liftIO $ newIORef []
    bd <- access llBoardL

    let write p v = do
          c <- readPos bd p
          liftIO $ modifyIORef rlog $ ((p, c):)
          liftIO $ modifyIORef wlog $ ((p, v):)
        commit = liftIO $ do
          wl <- readIORef wlog
          forM_ (reverse wl) $ \(p, v) -> writePos bd p v
          writeIORef wlog []

    moveC mv write
    commit
    update write
    commit

    diff <- liftIO $ readIORef rlog
    void $ llPatchesL %= \(p:ps) -> (p { pBoardDiff = diff }:ps)

type WriteLogger m = Pos -> Char -> LLT m ()

moveC :: (MonadIO m, Functor m) => Char -> WriteLogger m -> LLT m ()
moveC c = case c of
  'L' -> move $ Pos (-1) 0
  'R' -> move $ Pos 1    0
  'U' -> move $ Pos 0    1
  'D' -> move $ Pos 0    (-1)
  'W' -> \_ -> return ()
  'A' -> \_ -> return ()
  _   -> assert False undefined

move :: (MonadIO m, Functor m) => Pos -> WriteLogger m -> LLT m ()
move d@(Pos _ dy) wlog = do
  LLState {..} <- get

  let p0 = llPos
      p1 = p0 + d
      p2 = p1 + d

  c1 <- readPos llBoard p1
  c2 <- readPos llBoard p2

  case () of
    _ | c1 `elem` " .O\\" -> do
        -- move to empty space
        wlog p0 ' '
        wlog p1 'R'
        when (c1 == '\\') $ void $ llLambdasL += 1
        when (c1 == 'O')  $ void $ llResultL ~= Win
        llPosL ~= p1
        return ()
      | dy == 0 && c1 == '*' && c2 == ' ' -> do
        -- push rock
        wlog p0 ' '
        wlog p1 'R'
        wlog p2 '*'
        llPosL ~= p1
        llRockPosL %= map (\p -> if p == p1 then p2 else p)
        return ()
      | c1 `elem` ['A' .. 'I'] -> do
        -- move to trampoline
        let Just (_, to, erases) = lookup c1 llTramp
        wlog p0 ' '
        wlog p1 ' '
        wlog to 'R'
        forM_ erases $ \tp -> wlog tp ' '
        llPosL ~= to
        return ()
      | otherwise -> do
        return ()

update :: (Functor m, MonadIO m) => WriteLogger m -> LLT m ()
update wlog = do
  LLState {..} <- get

  bup <- readPos llBoard $ llPos + Pos 0 1

  -- lambda complete!
  when (llLambdas == llTotalLambdas) $ do
    wlog llLiftPos 'O'

  newRocks <- forM llRockPos $ \p -> do
    -- la [ca] ra
    -- lb  cb  rb
    let pla = p + Pos (-1) 0
        plb = p + Pos (-1) (-1)
        pca = p + Pos 0    0
        pcb = p + Pos 0    (-1)
        pra = p + Pos 1    0
        prb = p + Pos 1    (-1)

    la <- readPos llBoard pla
    lb <- readPos llBoard plb
    ca <- readPos llBoard pca
    cb <- readPos llBoard pcb
    ra <- readPos llBoard pra
    rb <- readPos llBoard prb

    case () of
      _ | ca == '*' &&
          cb == ' ' -> do
            wlog pca ' '
            wlog pcb '*'
            return pcb
        | ca == '*' && ra == ' ' &&
          cb == '*' && rb == ' ' -> do
            wlog pca ' '
            wlog prb '*'
            return prb
        | la == ' ' && ca == '*' &&
          lb == ' ' && cb == '*' -> do
            wlog pca ' '
            wlog plb '*'
            return plb
        | ca == '*'  && ra == ' ' &&
          cb == '\\' && rb == ' ' -> do
            wlog pca ' '
            wlog prb '*'
            return prb
      _ ->
        return pca

  llStepL += 1
  -- rocks must be sorted
  llRockPosL ~= sortBy (comparing $ \(Pos x y) -> (y, x)) newRocks

  when (bup /= '*' && any (== llPos + Pos 0 1) newRocks) $ do
    -- Totuzen no DEATH!!
    void $ llResultL ~= Dead

  let wl = Flood.waterLevel llStep llFlood
      ws | llPos < Pos wl minBound = llWaterStep + 1
         | otherwise = 0

  llWaterStepL ~= ws
  when (ws > Flood.waterproof llFlood) $ do
    void $ llResultL ~= Dead

-- patch utils

stash :: (MonadIO m, Functor m) => Char -> LLT m ()
stash mv = do
  LLState {..} <- get
  let revPatch = LLPatch
        { pMove = mv
        , pPrevPos = llPos
        , pPrevLambdas = llLambdas
        , pPrevRocks = llRockPos
        , pPrevWater = llWaterStep
        , pBoardDiff = []
        }
  void $ llPatchesL %= (revPatch:)

undo :: MonadIO m => LLT m ()
undo = do
  ps <- gets llPatches
  liftIO $ print $ length ps
  case ps of
    [] -> do
      liftIO $ putStrLn "cannot undo"
    (patch:_) -> do
      st  <- get
      st' <- liftIO $ st `unapply` patch
      put st'

unapply :: LLState -> LLPatch -> IO LLState
unapply st LLPatch {..} = do
  forM_ pBoardDiff $ \(pos, cell) -> writePos (llBoard st) pos cell
  return $ LLState
    { llTotalLambdas = llTotalLambdas st
    , llLiftPos = llLiftPos st
    , llFlood = llFlood st
    , llTramp = llTramp st

    , llResult = llResult st
    , llStep = llStep st - 1
    , llPos = pPrevPos
    , llLambdas = pPrevLambdas
    , llRockPos = pPrevRocks
    , llWaterStep = pPrevWater

    , llBoard = llBoard st
    , llPatches = drop 1 $ llPatches st
    }

-- backup and restore
backupState :: MonadIO m => LLT m LLState
backupState = do
  bd  <- access llBoardL
  nbd <- liftIO $ V.unsafeThaw =<< V.mapM UM.clone =<< V.unsafeFreeze bd
  st <- get
  return $ st { llBoard = nbd }

restoreState :: MonadIO m => LLState -> LLT m ()
restoreState = put

withBackup :: MonadIO m => LLT m a -> LLT m a
withBackup m = do
  st <- backupState
  ret <- m
  restoreState st
  return ret

-- misc

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

whenInBoundPos :: (U.Unbox x, MonadIO m)
                  => Field x -> Pos -> a -> IO a -> m a
whenInBoundPos bd (Pos x y) def action = liftIO $ do
  let h = GM.length bd
  w <- GM.length <$> GM.read bd 0
  if x >= 0 && x < w && y >= 0 && y < h
    then action
    else return def
{-# INLINEABLE whenInBoundPos #-}

readPos :: (Functor m, MonadIO m) => Board -> Pos -> m Char
readPos bd p = fromMaybe '#' <$> readPosM bd p
{-# INLINEABLE readPos #-}

readPosM :: (MonadPlus f, MonadIO m) => Board -> Pos -> m (f Char)
readPosM bd p@(Pos x y) = whenInBoundPos bd p mzero $ do
  row <- GM.read bd y
  return <$> GM.read row x
{-# INLINEABLE readPosM #-}

writePos :: MonadIO m => Board -> Pos -> Char -> m ()
writePos bd p@(Pos x y) v = whenInBoundPos bd p () $ do
  row <- GM.read bd y
  GM.write row x v
{-# INLINEABLE writePos #-}
