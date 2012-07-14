{-# LANGUAGE TemplateHaskell #-}

module State where

import Data.Lens.Template
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as UM

import Flood
import Pos

type Field a = VM.IOVector (UM.IOVector a)

type Board = Field Char

data LLState
  = LLState
    { llStep         :: Int
    , llLambdas      :: Int
    , llTotalLambdas :: Int
    , llFlood        :: Flood
    , llPos          :: Pos
    , llLiftPos      :: Pos
    , llRockPos      :: VM.IOVector Pos
    , llBoard        :: Board
    , llWaterStep    :: Int
    , llHist         :: [LLState]
    , llReplay       :: [Char]
    }

nameMakeLens ''LLState $ \name -> Just (name ++ "L")
