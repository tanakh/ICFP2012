module Flood
  ( Flood
  , readFlood
  ) where

type Flood = Flood { water :: Int, flooding :: Int, waterproof :: Int }

defaultFlood = Flood 0 0 10

readFlood :: [String] -> Flood
readFlood = foldr upd defaultFlood . map r where
  r s = let [name,sval] = words s
            val = read sval :: Int
        in (name, val)

upd ("Water", n) x = x { water = n }
upd ("Flooding", n) x = x { flooding = n }
upd ("Waterproof", n) x = x { waterproof = n }

