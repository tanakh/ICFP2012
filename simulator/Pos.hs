module Pos where

-- | Nanchatte Pos
--
-- >>> Pos 2 3 + Pos 2 (-1)
-- (4,2)
-- >>> 10 * Pos 5 6
-- (50,60)
-- >>> -Pos 4 3
-- (-4,-3)
-- >>> Pos 1.2 3
-- (1.2,3.0)
--

type Pos = PosOf Int
type Dpos = PosOf Double

data PosOf a = Pos { px :: a, py :: a } deriving (Eq, Ord)

instance Show a => Show (PosOf a) where
  show (Pos x y) = show (x,y)

norm :: (Num a) => PosOf a -> a
norm (Pos x y) = abs x + abs y

instance (Num a) => Num (PosOf a) where
  (Pos x1 y1) + (Pos x2 y2) = (x1+x2) `Pos` (y1+y2)
  (Pos x1 y1) - (Pos x2 y2) = (x1-x2) `Pos` (y1-y2)
  (Pos x1 y1) * (Pos x2 y2) = (x1*x2) `Pos` (y1*y2)
  abs (Pos x y) = error "abs Pos is undefined, use norm"
  negate (Pos x y) = negate x `Pos` negate y

  -- enable easy vector multiplication
  fromInteger i = let j = fromInteger i in Pos j j

