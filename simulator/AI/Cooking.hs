module AI.Cooking where

{- The recipe for generating best AIs -}

import Control.Applicative
import           Pos
import System.Random

newtype Wave = 
  Wave  
  [(Double, -- amplitude
    Double, -- freq
    Double -- initial phase
    )] deriving (Eq, Show, Read)

toAmp :: Double -> Wave -> Double
toAmp t (Wave xs) = 
  sum $ map (\(a,f,p0) -> a*cos(f*t+p0)) xs

toAmp2 :: Double -> Wave -> Dpos
toAmp2 t (Wave xs) = 
  sum $ map (\(a,f,p0) -> Pos (a*cos(f*t+p0)) (a*sin(f*t+p0)) ) xs


theRecipe :: Recipe
theRecipe = Recipe
 {
    windFreqMin = -4,
    windFreqMax = 0,
    weightFreqMin = -4,
    weightreqMax = 0,
    weightAmp = 1,
    searchAtomNum = 3,
    weightAtomNum = 5
         }
randomRecipe :: IO Recipe
randomRecipe = undefined

data Recipe = 
  Recipe {
    windFreqMin :: Double,
    windFreqMax :: Double,
    weightFreqMin :: Double,
    weightreqMax :: Double,
    weightAmp :: Double,
    searchAtomNum :: Double,
    weightAtomNum :: Double
         }

data Config = 
  Config 
  {
    searchAtom :: 
       [(Wave, -- weight
         String, -- source terrains
         String  -- passable terrains
        )],
    windAtom ::
       [(Wave, -- weight
         Wave -- direction
        )]
  } deriving (Eq, Show, Read)

randomConfig :: Recipe -> IO Config
randomConfig r = 
  Config <$> randomSearchAtom r <*> randomMany (weightAtomNum r) (randomWindAtom1 r)


choose :: [a] -> IO a
choose xs = do
  i <- randomRIO (0,length xs-1)
  return $ xs !! i

normalSearchAtom = 
    (Wave [(2.302, 0, 0)], "\\O", " .")

randomSearchAtom :: Recipe -> IO [(Wave, String, String)]
randomSearchAtom r = 
  (normalSearchAtom :) <$> randomMany 3 (randomSearchAtom1 r)

randomSearchAtom1 r = do
  w <- randomWeight r
  src <- choose ["\\O", "*"]
  pass <- choose [" "," ."," .\\"," .*"]
  return (w, src, pass)

randomWindAtom1 r = do
  w <- randomWeight r
  f  <- (*) <$> randomRIO (-1, 1) <*> (exp <$> randomRIO (-4, 0))
  p0 <- randomRIO (0, 2*pi)
  return (w, Wave [(1,f,p0)])

randomWeight ::  Recipe -> IO Wave
randomWeight r = Wave <$> randomMany 5 (randomWeight1 r)

randomWeight1 :: Recipe -> IO (Double, Double, Double)
randomWeight1 r = do
  a  <- randomRIO (0, 1)
  f  <- (*) <$> randomRIO (-1, 1) <*> (exp <$> randomRIO (-4, 0))
  p0 <- randomRIO (0, 2*pi)
  return (a,f,p0)


randomMany :: Double -> IO a -> IO [a]
randomMany num m = do
  x  <- m
  dice <- randomRIO (0,1)
  xs <- if dice > (1/num)
                then randomMany num m
                else return []
  return $ x:xs


