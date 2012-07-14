module Communicate where

import Control.Concurrent.STM

data Result
  = Win Int
  | Abort Int
  | Dead Int
  | Cont
  deriving (Show)


data Channel = Channel
  {
    submit :: TQueue (Result, String)
 } 