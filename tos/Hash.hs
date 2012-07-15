module Hash
  ( hashFull
  , diff, applyDiff, unapplyDiff
  ) where

import Data.Word
import Pos

type HashVal = Int64
newtype HashDiff = HashDiff Int64

p,a,b :: HashVal
p = 2^31-1
a = 16807
b = 194495824

hashChar :: Pos -> Char -> HashVal
hashChar (Pos x y) ch = (as!!x) * (bs!!y) * (fromIntegral $ ord ch)

as = iterate (\ n -> (n*a)`mod`p) 1
bs = iterate (\ n -> (n*b)`mod`p) 1

hashFull :: [(Pos,Char)] -> HashVal
hashFull = foldl' (+) . map (uncurry hashChar)

diff :: [(Pos,Char)] -> [(Pos,Char)] -> HashDiff
diff old new = HashDiff $ (hashFull new - hashFull old)`mod`p

applyDiff, unapplyDiff :: HashDiff -> HashVal -> HashVal
applyDiff (HashDiff d) old = (old+d)`mod`p
unapplyDiff (HashDiff d) new = (new-d)`mod`p
