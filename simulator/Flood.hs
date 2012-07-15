module Flood
  ( Flood (..)
  , readFlood
  , waterLevel
  ) where

data Flood
  = Flood
    { water :: {-# UNPACK #-} !Int
    , flooding :: {-# UNPACK #-} !Int
    , waterproof :: {-# UNPACK #-} !Int
    }

defaultFlood :: Flood
defaultFlood = Flood 0 0 10

waterLevel :: Int -> Flood -> Int
waterLevel step fld =   -- step is now 0-origin
  if flooding fld == 0
     then water fld
     else water fld + step `div` flooding fld

readFlood :: [String] -> Flood
readFlood = foldr upd defaultFlood . map r where
  r s = let [name,sval] = words s
            val = read sval :: Int
        in (name, val)

upd :: (String, Int) -> Flood -> Flood
upd ("Water", n) x = x { water = n }
upd ("Flooding", n) x = x { flooding = n }
upd ("Waterproof", n) x = x { waterproof = n }
