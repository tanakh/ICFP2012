{- Takusan Utsuto Jissai Atariyasui. -}
module Main(main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Loop
import Control.Monad.Trans
import Data.Lens
import Data.List
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
import System.Posix.Unistd

import           AI.Common
import           AI.GorinNoSho
import qualified Ans as Ans
import           DefaultMain
import           LL
import           Pos

newtype Wave = 
  Wave  
  [(Double, -- amplitude
    Double, -- freq
    Double -- initial phase
    )]
  
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
  }

main :: IO ()
main = do
  smellRef <- newIORef Nothing
  defaultMain $ simpleSolver smellRef $ 
    Config {
      searchAtom =
        [(Wave [], "\\O", " .")],
      windAtom = 
        [(Wave [(5, 0.05, 0),(5,0,3)], Wave [(1, 0.1, 0)])]
           }

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
  return $ Ans.Cont $ 
    snd $ last $ sort $  
    map (\(hand,val) -> (val,hand)) $
    Map.toList $ hyokaMap

