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
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import System.IO
import System.Process
import System.Posix.Time
import System.Posix.Signals
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


import Data.Word

data Resource =
  Resource {
    dijkstraMaps :: IORef (Map.Map String (Field Double)),
    submitter :: Tejun -> IO (),
    stepVisualize :: Bool,
    history :: IORef (Set.Set Word64)
           }
initResource :: IO Resource
initResource = Resource <$> newIORef (Map.empty) <*> pure (\_ -> return ()) <*> pure False <*> newIORef (Set.empty)

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
            when (Option.verbose opt) $ printe tejun,
          stepVisualize = (Option.verbose opt)
        }
      runLLT txt $ simpleSolver "" res config '_'
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
  seedTejun <- newTVarIO $ [Tejun 0 Abort "A"]
  population <- newTVarIO $ 0
  counter  <- newTVarIO $ 0
  learnedConfigs <- read <$> readFile "learned.txt"
  forkIO $ launcher population  counter seedTejun submitQ learnedConfigs txt
  forkIO $ collector opt submitQ bestTejun seedTejun
  termFlag <- newTVarIO False
  let hdlr = atomically $ writeTVar termFlag True
  installHandler sigINT (Catch hdlr) Nothing
  waitOhagi opt startTime bestTejun termFlag


launcher population counter seedTejun submitQ learnedConfigs txt = forever $ do
  pop <- atomically $ readTVar population
  when (pop < 30) $ do
    (Tejun _ _ seed) <- liftIO $ do
        seeds <- atomically $ readTVar seedTejun
        choose seeds
    res0 <- initResource
    let res = res0 { submitter = \x -> atomically $ writeTQueue submitQ x}
    let l2 :: [Config]
        l2 = learnedConfigs
    ctr <- atomically $ readTVar counter
    config0 <- randomConfig theRecipe
    config1 <- choose learnedConfigs
    let config 
           | ctr==0   = normalConfig 
           | even ctr = config0
           | True     = config1
    atomically $ modifyTVar counter (1+)
    forkIO $ do
      atomically $ modifyTVar population (1+)
      runLLT txt $ simpleSolver (filter (/='A')seed) res config '_'
      atomically $ modifyTVar population (1-)
    return ()
  when (pop > 10) $ usleep 1000

rejectTejun (Tejun sco _ rep) (Tejun sco2 _ rep2) = sco2 < sco && length rep2 > length rep

collector opt submitQ bestTejun seedTejun = forever $ do
  tejun <- atomically $ readTQueue submitQ
  atomically $ modifyTVar bestTejun (max tejun)
  atomically $ do
     xs <- readTVar seedTejun
     let xs2 
           | or  [rejectTejun x tejun|x<-xs] = xs
           | True                            = sort $ tejun:filter (not . rejectTejun tejun) xs
     writeTVar seedTejun xs2
--  seeds <- atomically $ readTVar seedTejun
--  liftIO $ hPutStrLn stderr$ unlines $ map show seeds

waitOhagi opt startTime bestTejun termFlag = do
  t <- getCurrentTime
  term <- atomically $ readTVar termFlag
  case () of
    _ | term || t > startTime + Option.timeout opt -2 -> do
          Tejun _ _ str <- atomically $ readTVar bestTejun
          putStrLn str
      | otherwise -> do
          sleep 1
          waitOhagi opt startTime bestTejun termFlag


isEffectiveMove :: (Functor m, MonadIO m) => Set.Set Word64 -> Char -> LLT m Bool
isEffectiveMove hist hand = do
  (res,h2) <- withStep hand $ do
    res' <- access llResultL
    h2'  <-access llHashL
    return (res', h2')
  let dies = res==Dead
  extraFlag <- if (hand/='S')
               then return True
               else do
                 withStep 'W' $ do
                     h3 <-access llHashL
                     return (h3 /= h2)
  return $ (not $ Set.member h2 hist) && not dies && extraFlag
simpleSolver :: (Functor m, MonadIO m)
                => String
                -> Resource
                -> Config
                -> Char
                -> LLT m Tejun
simpleSolver seed resource config lastStepChar = do
  bd <- access llBoardL
  hash <- access llHashL
  liftIO $ modifyIORef (history resource) $ Set.insert hash
  hist <- liftIO $ readIORef (history resource)
  validHands <- ('A':)<$> filterM (isEffectiveMove (hist)) "LRUDSW"
  let biasScore 'A' = -1e99
      biasScore 'S' =  1e99
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

  when (stepVisualize resource) showBoard

  when (seed == "") $ do
      -- treat each wind
      forM_ (windAtom config) $ \ (wave, windWave) -> do
        forM_ validHands $ \hand -> do
          let vec = posToDpos $ hand2pos hand
              windVec = toAmp2 time windWave
              val = (sinh . toAmp time) wave * (vec `innerProd` windVec)
          addHyoka hand val
      -- treat each search
      forM_ (searchAtom config) $ \ (wave, srcStr, passStr, pow) -> do
        dm <- liftIO $ readIORef $ dijkstraMaps resource
        let maybeSmell = Map.lookup srcStr dm
        smell <- case maybeSmell of
          Just a -> return a
          Nothing -> do
            b <- newF 0
            liftIO $ modifyIORef (dijkstraMaps resource) $
              Map.insert srcStr b
            return b
        when (True) $ dijkstra smell srcStr passStr 1
        updateF smell (\x -> if isPassable x then x**pow else 0)
        forM_ validHands $ \hand -> do
          smellAt <- unsafeReadF smell (roboPos + hand2pos hand)
          addHyoka hand $ (sinh . toAmp time) wave * smellAt
    
  hyokaMap <- liftIO $ readIORef hyokaRef
  let ansHand 
        | length seed > 0 = head seed
        | step > 1000 = 'A'
        | otherwise =
          snd $ last $ sort $
          map (\(hand,val) -> (val,hand)) $
          Map.toList $ hyokaMap
  nextStepChar <- liftIO $ readPos bd $ roboPos + hand2pos ansHand
  lam0 <- access llLambdasL
  simulateStep ansHand
  lam1 <- access llLambdasL
  when (lam1 /= lam0) $ do
    sco <- abortScore
    rep <- getReplay
    liftIO $ submitter resource $ Tejun sco Abort rep
  maybeS <- score
  case maybeS of
    Nothing -> simpleSolver(drop 1 seed) resource config nextStepChar
    Just sco  -> do
      res <- access llResultL
      rep <- getReplay
      let tejun = Tejun sco res rep
      liftIO $ submitter resource $ tejun
      return tejun