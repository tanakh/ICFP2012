{- Takusan Utsuto Jissai Atariyasui. -}
module Main(main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Loop
import Control.Monad.Trans
import Data.Lens
import Data.List
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.PQueue.Min as Q
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import System.IO
import System.Process
import System.Posix.Unistd
import System.Random
import System.FilePath
import Text.Printf

import           AI.Common
import           AI.GorinNoSho
import qualified Ans as Ans
import           DefaultMain
import           LL
import qualified Option as Opt
import           Pos

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

main :: IO ()
main = do
  smellRef <- newIORef Nothing
  config <- randomConfig
  res <- defaultMainRes $ simpleSolver smellRef $ config
  opt <- Opt.parseIO  
  let fnInput = case Opt.input opt of
        Opt.InputFile fp -> fp
        Opt.Stdin -> "STDIN"

  let fnRec = (printf "record/%s-%d-%s.txt"
            (dropExtension $ takeFileName fnInput)
            (scoreResult res)
            (take 6 $ show $ md5 $ L.pack $ show config)) 
  hPutStrLn stderr fnRec
  liftIO $ system "mkdir -p record/"
  liftIO $ writeFile fnRec $
    unlines $
      [show $ scoreResult res,
       show $ res,
       show $ config
      ]


randomConfig :: IO Config
randomConfig = 
  Config <$> randomSearchAtom <*> randomMany 5 randomWindAtom1


choose :: [a] -> IO a
choose xs = do
  i <- randomRIO (0,length xs-1)
  return $ xs !! i

normalSearchAtom = 
    (Wave [(2.302,0,0)], "\\O", " .")

randomSearchAtom :: IO [(Wave, String, String)]
randomSearchAtom = 
  (normalSearchAtom :) <$> randomMany 3 randomSearchAtom1

randomSearchAtom1 = do
  w <- randomWeight
  src <- choose ["\\O", "*"]
  pass <- choose [" "," ."," .\\"," .*"]
  return (w, src, pass)

randomWindAtom1 = do
  w <- randomWeight
  f  <- (*) <$> randomRIO (-1, 1) <*> (exp <$> randomRIO (-4, 0))
  p0 <- randomRIO (0, 2*pi)
  return (w, Wave [(1,f,p0)])

randomWeight :: IO Wave
randomWeight = Wave <$> randomMany 5 randomWeight1

randomWeight1 :: IO (Double, Double, Double)
randomWeight1 = do
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




isEffectiveMove :: (MonadIO m) => Char -> LLT m Bool
isEffectiveMove hand = return True

simpleSolver :: IORef (Maybe (Field Double)) -> Config -> Solver IO
simpleSolver smellRef config = safetynet $ do
  -- setup initial valmap, dijkstra field
  bd <- access llBoardL
  validHands <- filterM isEffectiveMove "LRUD"  
  -- dono te ga tsuyoinoka; watashi kininarimasu! 
  hyokaRef <- liftIO $ newIORef $ 
                Map.fromList [(hand,0::Double) | hand <- validHands]
  let addHyoka hand val =
        liftIO $ modifyIORef hyokaRef (Map.update (Just . (val+)) hand)
  roboPos <- access llPosL

  maybeSmell <- liftIO $ readIORef smellRef
  smell <- case maybeSmell of
    Just a -> return a
    Nothing -> do
      b <- newF (1::Double)
      liftIO $ writeIORef smellRef $ Just b
      return b

  step <- access llStepL
  let time :: Double
      time = fromIntegral step


  -- treat each wind
  forM_ (windAtom config) $ \ (wave, windWave) -> do
    forM_ validHands $ \hand -> do      
      let vec = fromIntegral <$> hand2pos hand
          windVec = toAmp2 time windWave
          val = (exp . toAmp time) wave * (vec `innerProd` windVec)    
      addHyoka hand val
  -- treat each search
  forM_ (searchAtom config) $ \ (wave, srcStr, passStr) -> do
    dijkstra smell srcStr passStr 1
    updateF smell (\x -> if isPassable x then 1/x else 0)
    -- showF (wideShow 5) smell --debug
    forM_ validHands $ \hand -> do
      smellAt <- unsafeReadF smell (roboPos + hand2pos hand)
      addHyoka hand $ (exp . toAmp time) wave * smellAt

  hyokaMap <- liftIO $ readIORef hyokaRef
  if step > 1000
     then return $ Ans.Cont 'A'
     else return $ Ans.Cont $ 
       snd $ last $ sort $  
       map (\(hand,val) -> (val,hand)) $
       Map.toList $ hyokaMap

