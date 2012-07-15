module AI.Common where

import Control.Monad.Trans
import System.IO

import LL
import Pos
import qualified Ans as Ans

hand2pos :: Char -> Pos
hand2pos 'L' =  Pos (-1)  0
hand2pos 'R' =  Pos   1   0
hand2pos 'U' =  Pos   0   1
hand2pos 'D' =  Pos   0 (-1)
hand2pos _   =  Pos   0   0

directions :: [(Char, Pos)]
directions = map (\c -> (c, hand2pos c)) "LRUD"
directions5 :: [(Char, Pos)]
directions5 = map (\c -> (c, hand2pos c)) "LRUDW"

safetynet :: (MonadIO m) => Solver m -> Solver m
safetynet m = do
  withBackup $ do
    ret <- m
    case ret of
      Ans.Cont ch -> if ch `elem` "LRUDWA"
                     then return ret
                     else do
                       liftIO $ hPutStrLn stderr $
                         "warning; AI used illegal command: " ++ show ret
                       return $ Ans.End
      Ans.End -> return $ Ans.Cont 'A'
      Ans.Undo ->  do
        liftIO $ hPutStrLn stderr $
          "warning; You cannot undo in Real world!"
        return $ Ans.Cont 'A'
