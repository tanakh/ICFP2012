{- Takusan Utsuto Jissai Atariyasui. -}
module Main(main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Loop
import Control.Monad.Trans
import Data.Lens
import Data.List
import qualified Data.PQueue.Min as Q
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import System.IO

import           AI.Common
import           AI.GorinNoSho
import qualified Ans as Ans
import           LL
import           DefaultMain

type Field a = VM.IOVector (VM.IOVector a) 

data Config = 
  Config 
  {
    serachPat :: [(String, String, Double)],
    oscillator :: [Double]
  }

main :: IO ()
main = defaultMain $ simpleSolver $ Config [] []

simpleSolver :: Config -> Solver IO
simpleSolver config = safetynet $ do
  smell <- dijkstra "\\O" " ." (1::Double) 
  updateF smell (\x -> if isPassable x then 1/x else 0)
  -- showF (wideShow 5) smell
  roboPos <- access llPosL
  evals <- forM directions $ \(char, vec) -> do
    vals <- readFList smell (roboPos + vec)
    return [(val, char) | val <- vals]
  return $ Ans.Cont $ snd $ last $ sort $ concat $ evals

