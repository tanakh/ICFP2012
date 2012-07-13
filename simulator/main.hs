import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Loop
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

import System.Environment

main :: IO ()
main = do
  [infile, ansfile] <- getArgs
  txt <- readFile infile
  let bd0 = lines txt
      w = maximum $ map length bd0
      bdl = map (take w . (++ repeat ' ')) bd0
  bd <- V.thaw . V.fromList =<< mapM (U.thaw . U.fromList) bdl
  mvs <- readFile ansfile
  simulate bd mvs
  undefined

simulate :: VM.IOVector (UM.IOVector Char)
            -> String
            -> IO Int
simulate bd mvs = do
  let h = GM.length bd
  w <- GM.length <$> GM.read bd 0
  (cx, cy) <- iterateLoopT 0 $ \y -> do
    iterateLoopT 0 $ \x -> do
      when (x >= w) exit
      row <- lift . lift $ GM.read bd y
      c   <- lift . lift $ GM.read row x
      when (c == 'R') $ lift $ exitWith (x, y)
      return $ x+1
    return $ y+1

  iterateLoopT (0, 0, cx, cy, mvs) $ \(step, lms ,cx, cy, (mv:mvs)) -> do
    let move dx dy = do
          let (nx, ny) = (cx + dx, cy + dy)
          if nx >= 0 && nx < w && ny >= 0 && ny < h
            then do
            c <- liftIO $ readCell bd nx ny
            if c `elem` " .O\\"
              then do
              liftIO $ writeCell bd cx cy ' '
              liftIO $ writeCell bd nx ny 'R'
              return (lms + if c == '\\' then 1 else 0, nx, ny)
              else do
              return (lms, cx, cy)
            else do
            return (lms, cx, cy)

    (lms, nx, ny) <- case mv of
      'L' -> move (-1) 0
      'R' -> move 1    0
      'U' -> move 0    1
      'D' -> move 0    (-1)
      'W' -> return (lms, cy, cy)
      'A' -> do
        liftIO $ putStrLn "Aborted"
        exitWith $ lms * 50 - step

    -- upadte
    nbd <- liftIO $ VM.replicateM h $ UM.replicate w ' '
    foreach [0..h-1] $ \y -> do
      foreach [0..w-1] $ \x -> do
        liftIO $ writeCell nbd x y =<< readCell bd x y
        when (j > 0) $ do
          undefined

    return (step + 1, lms, nx, ny, mvs)

  where
    readCell bd x y = do
      row <- GM.read bd y
      GM.read row x

    writeCell bd x y v = do
      row <- GM.read bd y
      GM.write row x v
