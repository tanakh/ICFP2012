module AI.Cooking where

{- The recipe for generating best AIs -}

import Control.Applicative
import Control.Monad
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
    weightFreqMax = 0,
    weightAmp = 1,
    searchAtomNum = 3,
    weightAtomNum = 5,
    possiblySource   = "O\\*" ,
    possiblyPass = " .\\*",
    powerMin = -1,
    powerMax = -1
         }
 
allChars = "R*L.#\\O @!WA1"

randomRecipe :: IO Recipe
randomRecipe = do
  dice <- randomRIO (0,1)
  if dice < (0.2::Double)
    then  return theRecipe
    else  randomRecipe1

randomRecipe1 :: IO Recipe
randomRecipe1 = Recipe 
  <$> randomRIO (-8, 4) 
  <*> randomRIO (-8, 4) 
  <*> randomRIO (-8, 4) 
  <*> randomRIO (-8, 4) 

  <*> randomRIO (0, 3) 

  <*> randomRIO (0, 20) 
  <*> randomRIO (0, 20) 
  <*> (choose $ powerset1 $ allChars)
  <*> (choose $ powerset1 $ allChars)
  <*> randomRIO (-4, 2) 
  <*> randomRIO (-4, 2) 
data Recipe = 
  Recipe {
    windFreqMin :: Double,
    windFreqMax :: Double,
    weightFreqMin :: Double,
    weightFreqMax :: Double,
    weightAmp :: Double,
    searchAtomNum :: Double,
    weightAtomNum :: Double,
    possiblySource :: String,
    possiblyPass :: String,
    powerMin :: Double,
    powerMax :: Double
         }

data Config = 
  Config 
  {
    searchAtom :: 
       [(Wave, -- weight
         String, -- source terrains
         String,  -- passable terrains
         Double -- power law index
        )],
    windAtom ::
       [(Wave, -- weight
         Wave -- direction
        )]
  } deriving (Eq, Show, Read)

randomConfig :: Recipe -> IO Config
randomConfig r = 
  Config <$> randomSearchAtom r <*> randomMany (weightAtomNum r) (randomWindAtom1 r)

normalConfig :: Config
normalConfig = Config [normalSearchAtom] []

choose :: [a] -> IO a
choose xs = do
  i <- randomRIO (0,length xs-1)
  return $ xs !! i

normalSearchAtom = 
    (Wave [(2.302, 0, 0)], "\\O", " .", -1)

randomSearchAtom :: Recipe -> IO [(Wave, String, String, Double)]
randomSearchAtom r = 
  (normalSearchAtom :) <$> randomMany (searchAtomNum r) (randomSearchAtom1 r)

powerset1 :: [a] -> [[a]]
powerset1 xs = filter ((>0). length) $ filterM (\x -> [True,False]) $ xs

randomSearchAtom1 r = do
  w <- randomWeight r
  src <- choose $ powerset1 $ possiblySource r
  pass <- choose $ powerset1 $ possiblyPass r
  pow <- randomRIO (powerMin r, powerMax r)
  return (w, src, pass, pow)

randomWindAtom1 r = do
  w <- randomWeight r
  f  <- (*) <$> randomRIO (-1, 1) <*> (exp <$> randomRIO (windFreqMin r, windFreqMax r))
  p0 <- randomRIO (0, 2*pi)
  return (w, Wave [(1,f,p0)])

randomWeight ::  Recipe -> IO Wave
randomWeight r = Wave <$> randomMany 5 (randomWeight1 r)

randomWeight1 :: Recipe -> IO (Double, Double, Double)
randomWeight1 r = do
  a  <- randomRIO (0, weightAmp r)
  f  <- (*) <$> randomRIO (-1, 1) <*> (exp <$> randomRIO (weightFreqMin r, weightFreqMax r))
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


