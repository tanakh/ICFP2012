{- Takusan Utsuto Jissai Atariyasui. -}
{-# OPTIONS -Wall #-}
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

main :: IO ()
main = defaultMain simpleSolver

simpleSolver :: Solver IO
simpleSolver = safetynet $ do
  field <- dijkstra "\\O" " ."
  return $ Ans.Cont 'A'

