{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE RecordWildCards, ViewPatterns, BangPatterns #-}

module LL (
  -- simulate whole game
  simulate,

  -- the LLT monad
  LLT, runLLT, Solver,

  -- exec step, and undo
  simulateStep, undo,
  withStep,
  isEnd, score, abortScore, deadScore, winScore,

  -- aux
  getSize,
  forPos, loopPos,
  whenInBoundPos,
  readPos, readPosM, writePos,
  getReplay,
  getAbortTejun,
  -- full backup, and restore (maybe heave...)
  backupState, restoreState, withBackup,

  -- diagnostic
  showStatus, showBoard,

  -- varidate inputs
  validInputs, isValidInput,

  module State,
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Strict
import Control.Monad.Trans.Loop
import Data.Bits
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
import Data.Word
import System.IO
import Text.Printf

import AI.Oracle(Oracle)
import qualified Ans
import qualified Flood
import Pos
import State

newtype LLT m a
  = LLT { unLLT :: StateT LLState m a }
  deriving ( Functor, Applicative
           , Monad, MonadIO
           , MonadState LLState, MonadTrans
           )

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
  return (lms * 50 - step + 1)

deadScore = do
  step <- access llStepL
  lms <- access llLambdasL
  return (lms * 25 - step)

-----

pr1, pr2, pr3 :: Word64
pr1 = 78123748972741237
pr2 = 8341975094375872387
pr3 = 67463278932786926713

hashChar :: Pos -> Char -> Word64
hashChar (Pos x y) c = do
  fromIntegral x * pr1 + fromIntegral y * pr2 +
    fromIntegral (fromEnum c) * pr3

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

  -- parse flood
  let fld = Flood.readFlood txtM

  -- parse trampoline
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

  -- parse beard
  let growth = head $ [ read g | ["Growth", g] <- map words txtM ] ++ [25]
      razors = head $ [ read r | ["Razors", r] <- map words txtM ] ++ [0]

  -- other info
  let rpos      = snd . head $ finds (=='R')
      lpos      = snd . head $ finds (=='L')
      lambdaNum = length $ finds (`elem` "\\@")
      rocks     = map snd $ finds isRock

  bd <- liftIO $ V.thaw . V.fromList =<< mapM (U.thaw . U.fromList) bdl
  let hash = foldl' xor 0
        [ hashChar (Pos x y) cell
        | (y, row)  <- zip [0..] bdl, (x, cell) <- zip [0..] row
        ]

  let initState = LLState
        { llTotalLambdas = lambdaNum
        , llLiftPos = lpos
        , llFlood = fld
        , llTramp = tramp
        , llGrowth = growth

        , llResult = Cont
        , llStep = 0
        , llPos = rpos
        , llLambdas = 0

        , llRockPos = sort rocks
        , llWaterStep = 0
        , llRazors = razors

        , llBoard = bd
        , llHash = hash

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

    printf "growth: %02d/%02d, razors: %d\n"
      (llGrowth - 1 - (llStep `mod` llGrowth))
      llGrowth
      llRazors

    when (not $ null llTramp) $ do
      putStrLn $ "trampoline: " ++
        ( intercalate ", "
          $ map (\(f, (t, _, _)) -> [f] ++ "->" ++ [t]) llTramp )

    printf "hash: %016X\n" llHash

  showBoard

showBoard :: MonadIO m => LLT m ()
showBoard = do
  LLState {..} <- get
  (w, h) <- getSize
  let wl = Flood.waterLevel llStep llFlood
  liftIO $ do
    forM_ [h-1, h-2 .. 0] $ \y -> do
      putStr =<< forM [0..w-1] (\x -> readPos llBoard $ Pos x y)
      putStrLn $ if y < wl then "~~~~" else "    "

getAbortTejun ::  (Functor m, MonadIO m) => LLT m Tejun
getAbortTejun = Tejun <$> abortScore <*> pure Abort <*> getReplay

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

withStep :: (Functor m, MonadIO m) => Char -> LLT m a -> LLT m a
withStep mv m = do
  simulateStep mv
  ret <- m
  undo
  return ret
{-# INLINEABLE withStep #-}

simulateStep :: (Functor m, MonadIO m) => Char -> LLT m ()
simulateStep mv = do
  stash mv

  if mv == 'A'
    then do
    llResultL ~= Abort
    return ()

    else do
    bd <- access llBoardL

    wlog <- liftIO $ newIORef []
    rlog <- liftIO $ newIORef []
    xlog <- liftIO $ newIORef 0

    let write p v = do
          c <- readPos bd p
          modifyIORef rlog $ ((p, c):)
          modifyIORef wlog $ ((p, v):)
          modifyIORef xlog (`xor` (hashChar p v `xor` hashChar p c))
        commit = do
          wl <- readIORef wlog
          forM_ (reverse wl) $ \(p, v) -> writePos bd p v
          writeIORef wlog []

    moveC mv write >> liftIO commit
    update write commit

    diff <- liftIO $ readIORef rlog
    hash <- liftIO $ readIORef xlog
    llHashL %= xor hash
    llPatchesL %= \(p:ps) -> (p { pBoardDiff = diff, pHash = hash }:ps)
    return ()

  llStepL += 1
  return ()

type WriteLogger = Pos -> Char -> IO ()
type Commit = IO ()

moveC :: (MonadIO m, Functor m) => Char -> WriteLogger -> LLT m ()
moveC c = case c of
  'L' -> move $ Pos (-1) 0
  'R' -> move $ Pos 1    0
  'U' -> move $ Pos 0    1
  'D' -> move $ Pos 0    (-1)
  'S' -> shave
  'W' -> \_ -> return ()
  _   -> assert False undefined

isRock :: Char -> Bool
isRock c = c == '*' || c == '@'

move :: (MonadIO m, Functor m) => Pos -> WriteLogger -> LLT m ()
move d@(Pos _ dy) wlog = do
  LLState {..} <- get

  let p0 = llPos
      p1 = p0 + d
      p2 = p1 + d

  c1 <- liftIO $ readPos llBoard p1
  c2 <- liftIO $ readPos llBoard p2

  case () of
    _ | c1 `elem` " .O\\!" -> do
        -- move to empty space
        liftIO $ wlog p0 ' '
        liftIO $ wlog p1 'R'
        when (c1 == '\\') $ void $ llLambdasL += 1
        when (c1 == 'O')  $ void $ llResultL ~= Win
        when (c1 == '!')  $ void $ llRazorsL += 1
        llPosL ~= p1
        return ()
      | dy == 0 && isRock c1 && c2 == ' ' -> do
        -- push rock
        liftIO $ wlog p0 ' '
        liftIO $ wlog p1 'R'
        liftIO $ wlog p2 c1
        llPosL ~= p1
        llRockPosL %= map (\p -> if p == p1 then p2 else p)
        return ()
      | c1 `elem` ['A' .. 'I'] -> do
        -- move to trampoline
        let Just (_, to, erases) = lookup c1 llTramp
        liftIO $ wlog p0 ' '
        liftIO $ wlog p1 ' '
        liftIO $ wlog to 'R'
        liftIO $ forM_ erases $ \tp -> wlog tp ' '
        llPosL ~= to
        return ()
      | otherwise -> do
        return ()

adjacent :: [Pos]
adjacent =
  [ Pos x y | x <- [-1 .. 1], y <- [-1 .. 1], not $ x == 0 && y == 0 ]

shave :: (MonadIO m, Functor m) => WriteLogger -> LLT m ()
shave wlog = do
  LLState {..} <- get
  when (llRazors > 0) $ do
    llRazorsL -= 1
    liftIO $ forM_ adjacent $ \d -> do
      let np = llPos + d
      cell <- readPos llBoard np
      when (cell == 'W') $ wlog np ' '

sortp :: [Pos] -> [Pos]
sortp = sortBy (comparing $ \(Pos x y) -> (y, x))

update :: (Functor m, MonadIO m) => WriteLogger -> Commit -> LLT m ()
update wlog commit = do
  LLState {..} <- get

  -- before update, what is above of robot?
  bup <- liftIO $ readPos llBoard $ llPos + Pos 0 1

  let growing = llStep `mod` llGrowth == llGrowth - 1

  cands <- {-# SCC "update/generateCands" #-}
    if not growing then return llRockPos
    else sortp . (llRockPos ++) . map fst <$> searchBoard (=='W')

  dior <- liftIO $ newIORef False
  rrocks <- liftIO $ newIORef []

  {-# SCC "update/updateRock" #-} liftIO $ forM_ cands $ \ !p -> do
    -- la [ca] ra
    -- lb  cb  rb
    -- lc  cc  rc
    let !pla = p + Pos (-1) 0
        !plb = p + Pos (-1) (-1)
        !plc = p + Pos (-1) (-2)
        !pca = p + Pos 0    0
        !pcb = p + Pos 0    (-1)
        !pcc = p + Pos 0    (-2)
        !pra = p + Pos 1    0
        !prb = p + Pos 1    (-1)
        !prc = p + Pos 1    (-2)

    !la <- readPos llBoard pla
    !lb <- readPos llBoard plb
    !lc <- readPos llBoard plc
    !ca <- readPos llBoard pca
    !cb <- readPos llBoard pcb
    !cc <- readPos llBoard pcc
    !ra <- readPos llBoard pra
    !rb <- readPos llBoard prb
    !rc <- readPos llBoard prc

    case () of
      _ | ca == 'W' -> do
            forM_ adjacent $ \((pca+) -> pw) -> do
              !cell <- readPos llBoard pw
              when (cell == ' ') $ wlog pw 'W'
            modifyIORef rrocks (pca:)

        | isRock ca &&
          cb == ' ' -> do
            let rr = if ca == '@' && cc /= ' ' then '\\' else ca
            wlog pca ' '
            wlog pcb rr
            when (cc == 'R') $ writeIORef dior True
            when (isRock rr) $ modifyIORef rrocks (pcb:)
        | isRock ca && ra == ' ' &&
          isRock cb && rb == ' ' -> do
            let rr = if ca == '@' && rc /= ' ' then '\\' else ca
            wlog pca ' '
            wlog prb rr
            when (rc == 'R') $ writeIORef dior True
            when (isRock rr) $ modifyIORef rrocks (prb:)
        | la == ' ' && isRock ca &&
          lb == ' ' && isRock cb -> do
            let rr = if ca == '@' && lc /= ' ' then '\\' else ca
            wlog pca ' '
            wlog plb rr
            when (lc == 'R') $ writeIORef dior True
            when (isRock rr) $ modifyIORef rrocks (plb:)
        | isRock ca  && ra == ' ' &&
          cb == '\\' && rb == ' ' -> do
            let rr = if ca == '@' && rc /= ' ' then '\\' else ca
            wlog pca ' '
            wlog prb rr
            when (rc == 'R') $ writeIORef dior True
            when (isRock rr) $ modifyIORef rrocks (prb:)

      _ ->
        when (ca == '*') $ modifyIORef rrocks (pca:)

  -- lambda complete!
  liftIO $ when (llLambdas == llTotalLambdas) $ do
    wlog llLiftPos 'O'

  -- before check some kind of thins, do commit
  liftIO commit

  -- sanitize rocks' pos
  -- newRocks' <- {-# SCC "update/sanitize" #-} liftIO $
  --   filterM (\p -> isRock <$> readPos llBoard p) newRocks

  -- rocks must be sorted
  newRocks <- liftIO $ readIORef rrocks
  llRockPosL ~= sortp newRocks

  cup <- liftIO $ readPos llBoard $ llPos + Pos 0 1
  dead <- liftIO $ readIORef dior
  when (dead || not (isRock bup) && isRock cup) $ do
    -- Totuzen no DEATH!!
    void $ llResultL ~= Dead

  let wl = Flood.waterLevel llStep llFlood
      Pos _ py = llPos
      ws | py < wl = llWaterStep + 1
         | otherwise = 0

  llWaterStepL ~= ws
  when (ws > Flood.waterproof llFlood) $ do
    void $ llResultL ~= Dead

  return ()

-- patch utils

stash :: (MonadIO m, Functor m) => Char -> LLT m ()
stash mv = do
  LLState {..} <- get
  let revPatch = LLPatch
        { pMove        = mv
        , pPrevResult  = llResult
        , pPrevPos     = llPos
        , pPrevLambdas = llLambdas
        , pPrevRocks   = llRockPos
        , pPrevWater   = llWaterStep
        , pPrevRazors  = llRazors
        , pBoardDiff   = []
        , pHash        = 0
        }
  void $ llPatchesL %= (revPatch:)

undo :: MonadIO m => LLT m ()
undo = do
  ps <- access llPatchesL
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
    { -- constant
      llTotalLambdas = llTotalLambdas st
    , llLiftPos = llLiftPos st
    , llFlood = llFlood st
    , llTramp = llTramp st
    , llGrowth = llGrowth st

      -- revert previous status
    , llResult    = pPrevResult
    , llStep      = llStep st - 1
    , llPos       = pPrevPos
    , llLambdas   = pPrevLambdas
    , llRockPos   = pPrevRocks
    , llWaterStep = pPrevWater
    , llRazors    = pPrevRazors

    , llBoard = llBoard st
    , llHash  = llHash st `xor` pHash

    , llPatches = tail $ llPatches st
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

whenInBoundPos :: U.Unbox x => Field x -> Pos -> a -> (x -> IO a) -> IO a
whenInBoundPos bd (Pos x y) def action = do
  if y >= 0 && y < GM.length bd
    then do
    row <- GM.unsafeRead bd y
    if x >= 0 && x < GM.length row
      then action =<< GM.unsafeRead row x
      else return def
    else return def
{-# INLINEABLE whenInBoundPos #-}

readPos :: Board -> Pos -> IO Char
readPos bd p = whenInBoundPos bd p '#' return
{-# INLINEABLE readPos #-}

readPosM :: MonadPlus f => Board -> Pos -> IO (f Char)
readPosM bd p = whenInBoundPos bd p mzero $ return . return
{-# INLINEABLE readPosM #-}

writePos :: MonadIO m => Board -> Pos -> Char -> m ()
writePos bd (Pos x y) v = liftIO $ do
  when (y >= 0 && y < GM.length bd) $ do
    row <- GM.unsafeRead bd y
    when (x >= 0 && x < GM.length row) $
      GM.unsafeWrite row x v
{-# INLINEABLE writePos #-}

searchBoard :: MonadIO m => (Char -> Bool) -> LLT m [(Pos, Char)]
searchBoard p = do
  bd <- access llBoardL
  ior <- liftIO $ newIORef []
  forPos $ \pos -> liftIO $ do
    cell <- readPos bd pos
    when (p cell) $
      modifyIORef ior ((pos, cell):)
  liftIO $ readIORef ior

validInputs :: [Char]
validInputs = "LRUDSWA"

isValidInput :: Char -> Bool
isValidInput = (`elem` validInputs)
