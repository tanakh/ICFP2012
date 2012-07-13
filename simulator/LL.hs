{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module LL where

import Control.Applicative
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

data Pos = Pos { px :: Int, py :: Int }

data LLState
  = LLState
    { llStep :: Int
    , llLambdas :: Int
    , llTotalLambdas :: Int
    , llPos :: Pos
    , llBoard :: VM.IOVector (UM.IOVector Char)
    }
nameMakeLens ''LLState $ \name -> Just (name ++ "L")

newtype LLT m a
  = LLT { unLLT :: StateT LLState m a }
  deriving ( Functor, Applicative
           , Monad, MonadIO
           , MonadState LLState, MonadTrans)

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

simulateStep :: (Functor m, Monad m, MonadIO m) => Char -> LLT m Result
simulateStep mv = do
  step <- access llStepL
  bd <- access llBoardL
  Pos cx cy <- access llPosL
  lms <- access llLambdasL
  lambdaNum <- access llTotalLambdasL

  let h = GM.length bd
  w <- liftIO $ GM.length <$> GM.read bd 0

  liftIO $ showBoard bd
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
                exitWith $ Win $ lms * 75 - (step + 1)
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

    liftIO $ print (lms, lambdaNum, nx, ny)

    -- upadte
    nbd <- liftIO $ VM.replicateM h $ UM.replicate w ' '
    foreach [0..h-1] $ \y -> do
      foreach [0..w-1] $ \x -> do
        liftIO $ writeCell nbd x y =<< readCell bd x y
        c <- liftIO $ readCell bd x y

        when (c == 'L') $ do
          when (lms == lambdaNum) $ do
            liftIO $ writeCell nbd x y 'O'
          continue

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

    a <- liftIO $ readCell bd (ny + 1) nx
    b <- liftIO $ readCell nbd (ny + 1) nx
    when (a /= '*' && b == '*') $ do -- DEATH!!
      liftIO $ putStrLn "You Died"
      exitWith $ Dead $ lms * 25 - (step + 1)

    when (mv == 'A') $ do
      liftIO $ putStrLn "Aborted"
      exitWith $ Abort $ lms * 50 - (step + 1)

    liftIO $ GM.move bd nbd

    lift $ do
      llStepL ~= step + 1
      llLambdasL ~= lms
      llPosL ~= Pos nx ny
      return Cont

simulate :: VM.IOVector (UM.IOVector Char)
            -> String
            -> IO Result
simulate bd mvs = do
  let h = GM.length bd
  w <- GM.length <$> GM.read bd 0
  (cx, cy) <- iterateLoopT 0 $ \y -> do
    iterateLoopT 0 $ \x -> do
      when (x >= w) exit
      c <- liftIO $ readCell bd x y
      when (c == 'R') $ lift $ exitWith (x, y)
      return $ x+1
    return $ y+1

  lambdaNum <- do
    ior <- newIORef 0
    forM_ [0..h-1] $ \y -> do
      forM_ [0..w-1] $ \x -> do
        c <- liftIO $ readCell bd x y
        when (c == '\\') $ modifyIORef ior (+1)
    readIORef ior

  let initState = LLState
        { llStep = 0
        , llLambdas = 0
        , llTotalLambdas = lambdaNum
        , llPos = Pos cx cy
        , llBoard = bd
        }
  (`evalStateT` initState) $ unLLT $ do
    once $ do
      foreach mvs $ \mv -> do
        res <- lift . lift $ simulateStep mv
        case res of
          Cont -> do
            continue
          _ -> do
            lift $ exitWith res
      exitWith Cont

readCell bd x y = do
  row <- GM.read bd y
  GM.read row x

writeCell bd x y v = do
  row <- GM.read bd y
  GM.write row x v

showBoard bd = do
  let h = GM.length bd
  w <- GM.length <$> GM.read bd 0
  forM_ [h-1, h-2 .. 0] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      putChar =<< readCell bd x y
    putStrLn ""
