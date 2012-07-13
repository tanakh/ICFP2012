module AI.Common where

import Control.Monad.Trans
import System.IO

import LL
import Pos
import qualified Ans as Ans

directions = [('L', Pos (-1) 0), 
              ('R', Pos 1 0), 
              ('U', Pos 0 1),
              ('D', Pos 0 (-1)) ]
             
             
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
                       