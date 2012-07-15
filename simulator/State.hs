{-# LANGUAGE TemplateHaskell #-}

module State where

import Data.Lens.Template
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as UM

import Flood
import Pos

type Field a = VM.IOVector (UM.IOVector a)

type Board = Field Char

data Result
  = Win
  | Abort
  | Dead
  | Cont
  deriving (Show, Eq)

data LLState
  = LLState
    { -- constants
      llTotalLambdas :: {-# UNPACK #-} !Int
    , llFlood        :: {-# UNPACK #-} !Flood
    , llLiftPos      :: {-# UNPACK #-} !Pos

      -- current state
    , llResult       :: {-# UNPACK #-} !Result
    , llStep         :: {-# UNPACK #-} !Int
    , llPos          :: {-# UNPACK #-} !Pos
    , llLambdas      :: {-# UNPACK #-} !Int

    , llRockPos      :: ![Pos]
    , llWaterStep    :: {-# UNPACK #-} !Int

      -- mutable, need for clone for backup
    , llBoard        :: !Board

      -- history
    , llPatches      :: ![LLPatch]
    }

data LLPatch
  = LLPatch
    { pMove        :: {-# UNPACK #-} !Char
    , pPrevPos     :: {-# UNPACK #-} !Pos
    , pPrevLambdas :: {-# UNPACK #-} !Int
    , pPrevRocks   :: ![Pos]
    , pPrevWater   :: {-# UNPACK #-} !Int
    , pBoardDiff   :: ![(Pos, Char)]
    }

nameMakeLens ''LLState $ \name -> Just (name ++ "L")
