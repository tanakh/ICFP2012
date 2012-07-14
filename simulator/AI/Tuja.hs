{- Takusan Utsuto Jissai Atariyasui. -}
module Main(main) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent
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
import System.Posix.Time
import System.Posix.Unistd
import System.FilePath
import Text.Printf

import           AI.Cooking
import           AI.Common
import           AI.GorinNoSho
import qualified Ans as Ans
import           DefaultMain
import qualified Flood
import           LL
import qualified Option as Option
import           Pos




data Resource = 
  Resource {
    dijkstraMaps :: IORef (Map.Map String (Field Double)),
    submitter :: Tejun -> IO ()
           }
initResource :: IO Resource
initResource = Resource <$> newIORef (Map.empty) <*> pure (\_ -> return ())

getCurrentTime :: IO Int
getCurrentTime = read . show <$> epochTime

main :: IO ()
main = do
  startTime <- getCurrentTime
  opt <- Option.parseIO  
  txt <- case Option.input opt of
    Option.Stdin -> getContents
    Option.InputFile fn -> readFile fn
  case Option.mode opt of
    Option.Ninja -> ninjaMain opt startTime txt
    Option.Survey -> do  
      res0 <- initResource
      recipe <- randomRecipe
      config <- randomConfig recipe
      tejunRef <- newIORef $ Tejun 0 Abort "A"
      let res = res0{
          submitter  = \tejun ->  do
            modifyIORef tejunRef (max tejun)
            when (Option.verbose opt) $ printe tejun
        }
      runLLT txt $ simpleSolver res config
      Tejun sco res rep <- readIORef tejunRef
      let fnInput = case Option.input opt of
            Option.InputFile fp -> fp
            Option.Stdin -> "STDIN"  
          fnRec = (printf "record/%s/%d-%s-%s.txt"
                (dropExtension $ takeFileName fnInput)
                (sco)
                (take 6 $ show $ md5 $ L.pack rep)
                (take 6 $ show $ md5 $ L.pack $ show config)) 
      when (Option.verbose opt) $ hPutStrLn stderr fnRec
      liftIO $ system $ "mkdir -p record/" ++ (dropExtension $ takeFileName fnInput) ++ "/"
      liftIO $ writeFile fnRec $
        unlines $
          [show $ sco,
           show $ res,
           show $ config,
           rep
          ]

ninjaMain :: Option.Option -> Int -> String -> IO ()
ninjaMain opt startTime txt = do
  submitQ <- newTQueueIO 
  bestTejun <- newTVarIO $ Tejun 0 Abort "A"
  population <- newTVarIO $ 0
  learnedConfigs <- read <$> readFile "learned.txt"
  forkIO $ launcher population submitQ learnedConfigs txt
  forkIO $ collector opt submitQ bestTejun
  waitOhagi opt startTime bestTejun

launcher population submitQ learnedConfigs txt = forever $ do
  pop <- atomically $ readTVar population
  when (pop < 30) $ do
    res0 <- initResource
    let res = res0 { submitter = \x -> atomically $ writeTQueue submitQ x}
    -- config <- randomConfig theRecipe
    config <- choose learnedConfigs
    forkIO $ do
      atomically $ modifyTVar population (1+)
      runLLT txt $ simpleSolver res config
      atomically $ modifyTVar population (1-)
    return ()
  when (pop > 10) $ usleep 1000
  
collector opt submitQ bestTejun = forever $do
  tejun <- atomically $ readTQueue submitQ
  atomically $ modifyTVar bestTejun (max tejun)
  best <- atomically $ readTVar bestTejun
  when (Option.verbose opt) $ do
    hPutStrLn stderr $ "receive " ++ show tejun
    hPutStrLn stderr $ "my best " ++ show best
  
waitOhagi opt startTime bestTejun = do
  t <- getCurrentTime
  case () of 
    _ | t > startTime + Option.timeout opt -2 -> do
          Tejun _ _ str <- atomically $ readTVar bestTejun
          putStrLn str
      | otherwise -> do
          sleep 1
          waitOhagi opt startTime bestTejun


isEffectiveMove :: (Functor m, MonadIO m) => Char -> LLT m Bool
isEffectiveMove hand = do
  h1 <- access llHashL
  (res,h2) <- withBackup $ do
    simulateStep hand
    res' <- access llResultL    
    h2'  <-access llHashL
    return (res', h2')
  let dies = res==Dead
  return $ h1/=h2 && not dies
simpleSolver :: (Functor m, MonadIO m) 
                => Resource
                -> Config 
                -> LLT m Tejun
simpleSolver resource config = do
  bd <- access llBoardL
  validHands <- ('A':)<$> filterM isEffectiveMove "LRUDSW"  
  let biasScore 'A' = -1e99
      biasScore 'S' = +1e99
      biasScore _   = 0
  -- dono te ga tsuyoinoka; watashi kininarimasu! 
  hyokaRef <- liftIO $ newIORef $ 
                Map.fromList [(hand, biasScore hand) | hand <- validHands]
  let addHyoka hand val =
        liftIO $ modifyIORef hyokaRef (Map.update (Just . (val+)) hand)
  roboPos <- access llPosL

  step <- access llStepL
  let time :: Double
      time = fromIntegral step


  -- treat each wind
  forM_ (windAtom config) $ \ (wave, windWave) -> do
    forM_ validHands $ \hand -> do      
      let vec = fromIntegral <$> hand2pos hand
          windVec = toAmp2 time windWave
          val = (sinh . toAmp time) wave * (vec `innerProd` windVec)    
      addHyoka hand val
  -- treat each search
  forM_ (searchAtom config) $ \ (wave, srcStr, passStr) -> do
    dm <- liftIO $ readIORef $ dijkstraMaps resource
    let maybeSmell = Map.lookup srcStr dm
    smell <- case maybeSmell of
      Just a -> return a
      Nothing -> do
        b <- newF 0
        liftIO $ modifyIORef (dijkstraMaps resource) $ 
          Map.insert srcStr b
        return b
    dijkstra smell srcStr passStr 1
    updateF smell (\x -> if isPassable x then 1/x else 0)
    forM_ validHands $ \hand -> do
      smellAt <- unsafeReadF smell (roboPos + hand2pos hand)
      addHyoka hand $ (sinh . toAmp time) wave * smellAt

  hyokaMap <- liftIO $ readIORef hyokaRef
  let ansHand = 
        if step > 1000
        then 'A'
        else 
          snd $ last $ sort $  
          map (\(hand,val) -> (val,hand)) $
          Map.toList $ hyokaMap
  
  lam0 <- access llLambdasL
  simulateStep ansHand
  lam1 <- access llLambdasL
  when (lam1 /= lam0) $ do
    sco <- abortScore
    rep <- getReplay
    liftIO $ submitter resource $ Tejun sco Abort rep
  maybeS <- score
  case maybeS of
    Nothing -> simpleSolver resource config
    Just sco  -> do
      res <- access llResultL
      rep <- getReplay
      let tejun = Tejun sco res rep
      liftIO $ submitter resource $ tejun
      return tejun