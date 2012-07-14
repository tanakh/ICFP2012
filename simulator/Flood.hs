module Flood
  ( Flood (..)
  , readFlood
  , waterLevel
  ) where

data Flood = Flood { water :: Int, flooding :: Int, waterproof :: Int }

defaultFlood = Flood 0 0 10

waterLevel :: Int -> Flood -> Int
waterLevel step fld =   -- step is 1-origin
  if flooding fld == 0
     then water fld
     else water fld + (step-1) `div` flooding fld

readFlood :: [String] -> Flood
readFlood = foldr upd defaultFlood . map r where
  r s = let [name,sval] = words s
            val = read sval :: Int
        in (name, val)

upd ("Water", n) x = x { water = n }
upd ("Flooding", n) x = x { flooding = n }
upd ("Waterproof", n) x = x { waterproof = n }

