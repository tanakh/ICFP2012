import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Loop
import Data.IORef
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
  let bd0 = reverse $ lines txt
      w = maximum $ map length bd0
      bdl = map (take w . (++ repeat ' ')) bd0
  bd <- V.thaw . V.fromList =<< mapM (U.thaw . U.fromList) bdl
  mvs <- filter (`elem` "LRUDWA") <$> readFile ansfile
  simulate bd $ mvs ++ "X"
  showBoard bd

simulate :: VM.IOVector (UM.IOVector Char)
            -> String
            -> IO Int
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

  ior <- newIORef 0
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      c <- liftIO $ readCell bd x y
      when (c == '\\') $ modifyIORef ior (+1)
  lambdaNum <- readIORef ior

  iterateLoopT (0, 0, cx, cy, mvs) $ \(step, lms ,cx, cy, (mv:mvs)) -> do
    when (mv == 'X') $ exitWith 0

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

    liftIO $ GM.move bd nbd
    return (step + 1, lms, nx, ny, mvs)

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
