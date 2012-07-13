{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module LL where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Loop
import Data.IORef
import Data.Lens
import Data.Lens.Template
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Text.Printf

import qualified Ans as Ans
import qualified Option as Opt
import Pos

type Board = VM.IOVector (UM.IOVector Char)

data LLState
  = LLState
    { llStep         :: Int
    , llLambdas      :: Int
    , llTotalLambdas :: Int
    , llPos          :: Pos
    , llBoard        :: Board
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

runLLT :: MonadIO m => [String] -> LLT m a -> m a
runLLT bdl m = do
  let h = length bdl
      w = length $ head bdl
  bd <- liftIO $ V.thaw . V.fromList =<< mapM (U.thaw . U.fromList) bdl

  (cx, cy) <- iterateLoopT 0 $ \y -> do
    iterateLoopT 0 $ \x -> do
      when (x >= w) exit
      c <- liftIO $ readCell bd x y
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
        { llStep = 0
        , llLambdas = 0
        , llTotalLambdas = lambdaNum
        , llPos = Pos cx cy
        , llBoard = bd
        }
  evalStateT (unLLT m) initState

data Result
  = Win Int
  | Abort Int
  | Dead Int
  | Cont
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
  liftIO $ do
    let h = GM.length bd
    w <- GM.length <$> GM.read bd 0
    forM_ [h-1, h-2 .. 0] $ \y -> do
      forM_ [0..w-1] $ \x -> do
        putChar =<< readCell bd x y
      putStrLn ""

simulate :: Opt.Option -> [String] -> MVar Ans.Ans -> MVar LLState -> IO Result
simulate opt bd mVarAns mVarState = do
  runLLT bd $ do
    res <- once $ do
      repeatLoopT $ do
        -- pass the current status to the provider (and to player)
        st <- lift . lift $ get
        liftIO $ putMVar mVarState st
        when (Opt.verbose opt) $ lift . lift $ showStatus
        -- receive answer
        ans <- liftIO $ takeMVar mVarAns
        let mv = case ans of
              Ans.End -> 'A'
              Ans.Cont ch -> ch
        res <- lift . lift $ simulateStep mv

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
    return res

simulateStep :: (Functor m, Monad m, MonadIO m) => Char -> LLT m Result
simulateStep mv = do
  step <- access llStepL
  bd <- access llBoardL
  Pos cx cy <- access llPosL
  lms <- access llLambdasL
  lambdaNum <- access llTotalLambdasL

  let h = GM.length bd
  w <- liftIO $ GM.length <$> GM.read bd 0

  let move dx dy = do
          let (nx, ny) = (cx + dx, cy + dy)
          if nx >= 0 && nx < w && ny >= 0 && ny < h
            then do
            c <- liftIO $ readCell bd nx ny
            if c `elem` " .O\\"
              then do
              liftIO $ writeCell bd cx cy ' '
              liftIO $ writeCell bd nx ny 'R'
              when (c == 'O') $
                exitWith $ Win $ lms * 75 - step
              return (lms + if c == '\\' then 1 else 0, nx, ny)
              else do
              return (lms, cx, cy)
            else do
            return (lms, cx, cy)

  once $ do
    (lms, nx, ny) <- case mv of
      'L' -> move (-1) 0
      'R' -> move 1    0
      'U' -> move 0    1
      'D' -> move 0    (-1)
      'W' -> return (lms, cx, cy)
      'A' -> return (lms, cx, cy)

    -- upadte
    nbd <- liftIO $ VM.replicateM h $ UM.replicate w ' '
    foreach [0..h-1] $ \y -> do
      foreach [0..w-1] $ \x -> do
        liftIO $ writeCell nbd x y =<< readCell bd x y
        c <- liftIO $ readCell bd x y

        when (c == 'L' && lms == lambdaNum) $
          liftIO $ writeCell nbd x y 'O'
        when (c /= '*') continue

        when (y > 0) $ do
          b <- liftIO $ readCell bd x (y - 1)
          when (b == ' ') $ do
            liftIO $ writeCell nbd x (y - 1) '*'
            liftIO $ writeCell nbd x y ' '
            continue
          when (b == '*') $ do
            when (x + 1 < w) $ do
              c1 <- liftIO $ readCell bd (x + 1) y
              b1 <- liftIO $ readCell bd (x + 1) (y - 1)
              when (c1 == ' ' && b1 == ' ') $ do
                liftIO $ writeCell nbd (x + 1) (y - 1) '*'
                liftIO $ writeCell nbd x y ' '
                continue
            when (x - 1 >= 0) $ do
              c0 <- liftIO $ readCell bd (x - 1) y
              b0 <- liftIO $ readCell bd (x - 1) (y - 1)
              when (c0 == ' ' && b0 == ' ') $ do
                liftIO $ writeCell nbd (x - 1) (y - 1) '*'
                liftIO $ writeCell nbd x y ' '
                continue
          when (b == '\\' && x + 1 < w) $ do
            c1 <- liftIO $ readCell bd (x + 1) y
            b1 <- liftIO $ readCell bd (x + 1) (y - 1)
            when (c1 == ' ' && b1 == ' ') $ do
              liftIO $ writeCell nbd (x + 1) (y - 1) '*'
              liftIO $ writeCell nbd x y ' '
              continue

    a <- liftIO $ readCell bd  nx (ny + 1)
    b <- liftIO $ readCell nbd nx (ny + 1)
    when (a /= '*' && b == '*') $ do -- DEATH!!
      liftIO $ GM.move bd nbd
      exitWith $ Dead $ lms * 25 - step

    when (mv == 'A') $ do
      liftIO $ GM.move bd nbd
      exitWith $ Abort $ lms * 50 - step

    liftIO $ GM.move bd nbd
    lift $ do
      llStepL += 1
      llLambdasL ~= lms
      llPosL ~= Pos nx ny
      return Cont

readCell bd x y = do
  row <- GM.read bd y
  GM.read row x

writeCell bd x y v = do
  row <- GM.read bd y
  GM.write row x v
