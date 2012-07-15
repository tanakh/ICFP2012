{-# LANGUAGE TemplateHaskell #-}

module State where

import Data.Word
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
  deriving (Show, Eq, Ord,Read)

data LLState
  = LLState
    { -- constants
      llTotalLambdas :: {-# UNPACK #-} !Int
    , llLiftPos      :: {-# UNPACK #-} !Pos
    , llFlood        :: !Flood
    , llTramp        :: ![(Char, (Char, Pos, [Pos]))]
    , llGrowth       :: {-# UNPACK #-} !Int

      -- current state
    , llResult       :: {-# UNPACK #-} !Result
    , llStep         :: {-# UNPACK #-} !Int
    , llPos          :: {-# UNPACK #-} !Pos
    , llLambdas      :: {-# UNPACK #-} !Int

    , llRockPos      :: ![Pos]
    , llWaterStep    :: {-# UNPACK #-} !Int
    , llRazors       :: {-# UNPACK #-} !Int

      -- mutable, need for clone for backup
    , llBoard        :: !Board
    , llHash         :: {-# UNPACK #-} !Word64

      -- history
    , llPatches      :: ![LLPatch]
    }

data LLPatch
  = LLPatch
    { pMove        :: {-# UNPACK #-} !Char
    , pPrevResult  :: {-# UNPACK #-} !Result
    , pPrevPos     :: {-# UNPACK #-} !Pos
    , pPrevLambdas :: {-# UNPACK #-} !Int
    , pPrevRocks   :: ![Pos]
    , pPrevWater   :: {-# UNPACK #-} !Int
    , pPrevRazors  :: {-# UNPACK #-} !Int
    , pBoardDiff   :: ![(Pos, Char)]
    , pHash        :: {-# UNPACK #-} !Word64
    }

nameMakeLens ''LLState $ \name -> Just (name ++ "L")
