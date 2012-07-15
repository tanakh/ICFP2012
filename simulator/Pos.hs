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

data Pos
  = Pos
    { px :: {-# UNPACK #-} !Int
    , py :: {-# UNPACK #-} !Int }
  deriving (Eq, Ord)

data Dpos
  = Dpos
    { dpx :: {-# UNPACK #-} !Double
    , dpy :: {-# UNPACK #-} !Double }
  deriving (Eq, Ord)

instance Show Pos where
  show (Pos x y) = show (x,y)

norm :: Pos -> Int
norm (Pos x y) = abs x + abs y
{-# INLINEABLE norm #-}

innerProd :: Dpos -> Dpos -> Double
innerProd (Dpos x1 y1)  (Dpos x2 y2) = (x1*x2) + (y1*y2)
{-# INLINEABLE innerProd #-}

instance Num Pos where
  (Pos x1 y1) + (Pos x2 y2) = (x1+x2) `Pos` (y1+y2)
  (Pos x1 y1) - (Pos x2 y2) = (x1-x2) `Pos` (y1-y2)
  (Pos x1 y1) * (Pos x2 y2) = (x1*x2) `Pos` (y1*y2)
  abs _ = error "abs Pos is undefined, use norm"
  negate (Pos x y) = negate x `Pos` negate y
  signum _ = error "pos of signum"

  -- enable easy vector multiplication
  fromInteger i = let j = fromInteger i in Pos j j

instance Num Dpos where
  (Dpos x1 y1) + (Dpos x2 y2) = (x1+x2) `Dpos` (y1+y2)
  (Dpos x1 y1) - (Dpos x2 y2) = (x1-x2) `Dpos` (y1-y2)
  (Dpos x1 y1) * (Dpos x2 y2) = (x1*x2) `Dpos` (y1*y2)
  abs _ = error "abs Dpos is undefined, use norm"
  negate (Dpos x y) = negate x `Dpos` negate y
  signum _ = error "pos of signum"

  -- enable easy vector multiplication
  fromInteger i = let j = fromInteger i in Dpos j j

dposToPos :: Dpos -> Pos
dposToPos (Dpos x y) = Pos (floor x) (floor y)

posToDpos :: Pos -> Dpos
posToDpos (Pos x y) = Dpos (fromIntegral x) (fromIntegral y)
