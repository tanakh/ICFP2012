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
import System.Random
import System.FilePath
import Text.Printf

import           AI.Common
import           AI.GorinNoSho
import qualified Ans as Ans
import           DefaultMain
import qualified Flood
import           LL
import qualified Option as Option
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

randomConfig :: IO Config
randomConfig = 
  Config <$> randomSearchAtom <*> randomMany 5 randomWindAtom1


choose :: [a] -> IO a
choose xs = do
  i <- randomRIO (0,length xs-1)
  return $ xs !! i

normalSearchAtom = 
    (Wave [(2.302, 0, 0)], "\\O", " .")

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



data Tejun = Tejun Int Result String deriving (Eq,Ord,Show,Read)

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
  let (txtB,txtM) = span (/="") $ lines txt
      bd0 = reverse txtB
      w = maximum $ map length bd0
      bd = map (take w . (++ repeat ' ')) bd0
      fld = Flood.readFlood $ drop 1 txtM
  case Option.mode opt of
    Option.Ninja -> ninjaMain opt startTime  fld bd
    Option.Survey -> do  
      res0 <- initResource
      config <- randomConfig  
      let res 
            | Option.verbose opt = res0{submitter = printe}
            | otherwise          = res0
      (Tejun sco res rep) <- runLLT fld bd $ simpleSolver res config
      let fnInput = case Option.input opt of
            Option.InputFile fp -> fp
            Option.Stdin -> "STDIN"  
          fnRec = (printf "record/%s-%d-%s-%s.txt"
                (dropExtension $ takeFileName fnInput)
                (sco)
                (take 6 $ show $ md5 $ L.pack rep)
                (take 6 $ show $ md5 $ L.pack $ show config)) 
      hPutStrLn stderr fnRec
      liftIO $ system "mkdir -p record/"
      liftIO $ writeFile fnRec $
        unlines $
          [show $ sco,
           show $ res,
           show $ config,
           rep
          ]

ninjaMain :: Option.Option -> Int -> Flood.Flood -> [String] -> IO ()
ninjaMain opt startTime fld bd = do
  submitQ <- newTQueueIO 
  bestTejun <- newTVarIO $ Tejun 0 Abort "A"
  population <- newTVarIO $ 0
  forkIO $ launcher population submitQ fld bd
  forkIO $ collector opt submitQ bestTejun
  waitOhagi opt startTime bestTejun

launcher population submitQ fld bd = forever $ do
  pop <- atomically $ readTVar population
  when (pop < 30) $ do
    res0 <- initResource
    let res = res0 { submitter = \x -> atomically $ writeTQueue submitQ x}
    config <- randomConfig
    forkIO $ do
      atomically $ modifyTVar population (1+)
      runLLT fld bd $ simpleSolver res config
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
    _ | t > startTime + Option.timeout opt -> do
          Tejun _ _ str <- atomically $ readTVar bestTejun
          putStrLn str
      | otherwise -> do
          sleep 1
          waitOhagi opt startTime bestTejun


isEffectiveMove :: (MonadIO m) => Char -> LLT m Bool
isEffectiveMove hand = return True

simpleSolver :: (Functor m, MonadIO m) 
                => Resource
                -> Config 
                -> LLT m Tejun
simpleSolver resource config = do
  bd <- access llBoardL
  validHands <- filterM isEffectiveMove "LRUD"  
  -- dono te ga tsuyoinoka; watashi kininarimasu! 
  hyokaRef <- liftIO $ newIORef $ 
                Map.fromList [(hand,0::Double) | hand <- validHands]
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