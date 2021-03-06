{-# LANGUAGE TupleSections, ViewPatterns, RecordWildCards, BangPatterns #-}

module Main (main) where

import Control.Applicative
import Control.Monad

import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Vector.Mutable as VM
import Data.List
import qualified Data.HashSet as HS
import Data.Lens
import Data.Word
import Data.Ord
import System.IO
import System.Random
import System.Posix.Unistd

import Ans
import AI.Common
import qualified AI.Oracle as Oracle
import DefaultMain
import LL
import AI.GorinNoSho
import AI.Oracle
import qualified Option
import AI.Cooking(choose)
import Pos

minf :: Int
minf = -10^(8::Int)

moves :: [Char]
moves = "LRUDWSA"

type LL = LLT IO

type Cache = IORef (HM.HashMap Word64 [CacheEntry])

data CacheEntry
  = CacheEntry
    { ceStep      :: {-# UNPACK #-} !Int
    , ceWaterStep :: {-# UNPACK #-} !Int
    , ceRazors    :: {-# UNPACK #-} !Int
    , cePenalty   :: {-# UNPACK #-} !Int
    , ceScore     :: {-# UNPACK #-} !Int
    }

type History = IORef (HM.HashMap Word64 Int)

readHistory :: History -> Word64 -> IO Int
readHistory hist key = do
  hm <- readIORef hist
  case HM.lookup key hm of
    Just x  -> return x
    Nothing -> do
      modifyIORef hist $ HM.insert key 0
      return 0

modifyHistory :: History -> Word64 -> (Int -> Int)->IO ()
modifyHistory hist key f = do
  x <- readHistory hist key
  modifyIORef hist $ HM.insert key (f x)

isWorseThan :: CacheEntry -> CacheEntry -> Bool
a `isWorseThan` b =
  (ceScore a, - cePenalty a) <= (ceScore b, - cePenalty b)
  && ceStep a >= ceStep b
  && ceWaterStep a >= ceWaterStep b
  && ceRazors a <= ceRazors b
  -- && (ceScore a, 0) <= (ceScore b, 0)

addCacheEntry :: IORef (HM.HashMap Word64 [CacheEntry]) -> LL Int -> LL Int
addCacheEntry hmr m = do
  st <- get

  hm <- liftIO $ readIORef hmr
  let ce = CacheEntry
             { ceStep = llStep st
             , ceWaterStep = llWaterStep st
             , ceRazors = llRazors st
             , cePenalty = llPenalty st
             , ceScore = llLambdas st
             }
  let cm = case HM.lookup (llHash st) hm of
        Just cfs
          | not $ any (ce `isWorseThan`) cfs -> Nothing
          | otherwise ->
            return minf
            -- Just $ maximum $ map ceScore cfs
        _ -> Nothing
  case cm of
    Nothing -> do
      liftIO $ modifyIORef hmr $ HM.insertWith (++) (llHash st) [ce]
      sc <- m
      --liftIO $ modifyIORef hmr $ HM.insertWith (++) (llHash st)
      --  [ce { ceScore = sc }]
      return sc
    Just _ ->
      return minf


badnessCheck :: History -> History -> Int ->  Char -> LL Int
badnessCheck hist hyperhist fuel hand
  | fuel <= 0 = withStep hand $ do
                       res <- access llResultL
                       h <- access llHashL
                       step <- access llStepL
                       cnt <- liftIO $ readHistory hist h
                       cnt2 <- liftIO $ readHistory hyperhist h
                       return $ case () of
                                  _ | res == Dead        ->  3
                                    | cnt > 0            ->  2
                                    | cnt2*step >  0     ->  1
                                    | True               ->  0
  | otherwise = withStep hand $ do
                    gs <- (forM "LRUD" $ badnessCheck hist hyperhist (fuel-1))
                    return $ minimum gs


adjR =
  [ Pos 1 0
  , Pos 0 1
  , Pos (-1) 0
  ]

adj4 =
  [ Pos (-1) 0
  , Pos 1 0
  , Pos 0 (-1)
  , Pos 0 1
  ]
adj4m = "LRUD"

simplify :: Bool -> LL ()
simplify verboseSwitch = do
  bd <- access llBoardL
  forPos $ \p -> do
    cell <- liftIO $ readPos bd p
    liftIO $ when (cell == '.') $ do
      keepR <- forM adjR $ \((+p) -> n) -> do
        nell <- readPos bd n
        return $ isRock nell
      keepW <- forM adjacent $ \((+p) -> n) -> do
        nell <- readPos bd n
        return $ nell == 'W'
      when (not $ or keepR || or keepW) $ do
        writePos bd p ' '
  when (verboseSwitch) $ do
    liftIO $ putStrLn "simplify to:"
    showBoard

main :: IO ()
main = do
  opt <- Option.parseIO
  txt <- case Option.input opt of
    Option.Stdin -> getContents
    Option.InputFile fn -> readFile fn
  historyRef <- newIORef HS.empty
  valueFieldRef <- newIORef undefined
  loveFieldRef <- newIORef undefined
  hashLogRef <- newIORef []
  yomiRef <- newIORef (-1)
  let inputfn = case Option.input opt of
            Option.InputFile fp -> fp
            Option.Stdin -> "STDIN"
  oracle <- Oracle.new inputfn


  when (Option.oracleSource opt/= "") $ do
    Oracle.load oracle $ Option.oracleSource opt
  hyperHistory <- newIORef HM.empty


  infiniteLoop <- liftIO $ Oracle.ask oracle "infiniteLoop" $ 
    return True
  (if infiniteLoop then forever else id) $ do
    -- generate randomize parameters
    motionWeight <- forM "LRUD" $ \char -> do
      w <- randomRIO (0.3, 2.0) -- !!!!!!!
      return (char, w)
    history <- newIORef HM.empty
    earthDrugAmp <- liftIO $ Oracle.ask oracle "earthDrugAmp" $ return (0.0::Double)
    earthDrug <- exp <$> randomRIO (- earthDrugAmp, earthDrugAmp)
    itemLoveAmp <- liftIO $ Oracle.ask oracle "itemLoveAmp" $ return (0.0:: Double)
    placeLoveAmp <- liftIO $ Oracle.ask oracle "placeLoveAmp" $ return (0.0:: Double)
    placeLoveNum <- liftIO $ Oracle.ask oracle "placeLoveNum" $ return (0.0:: Double)
    tetete@(Tejun osco _ _ ) <- atomically $ readTVar (Oracle.tejunVar oracle)

    initYomi <- liftIO $ Oracle.ask oracle "initBFDepth" $ return (1::Int)
    let updateYomi x
         | x < 0                  =  initYomi
         | osco <= 10 && x <= 10  =  2+x 
         | otherwise              =  1+x
    liftIO $ modifyIORef yomiRef (updateYomi)
    yomi <- liftIO $ readIORef yomiRef
    --- start of One Challenge
    defaultMain txt oracle $ do
      step <- access llStepL
      (liftIO . Oracle.submit oracle) =<< getAbortTejun

      (w,h) <- getSize
      yomiDepth <- liftIO $ readIORef yomiRef
      hmr <- liftIO $ newIORef HM.empty

      hashNow <- access llHashL
      kabutta <- (>2) <$> (liftIO $ readHistory history hashNow)
      liftIO $ modifyHistory history hashNow (1+)

      liftIO $ do
        useHyperHistory <- Oracle.ask oracle "useHyperHistory" $ return False
        when (useHyperHistory) $ modifyHistory hyperHistory hashNow (1+)

      (mov, sc) <- withBackup $ do
        simplify (Option.verbose opt)
        rs <- forM moves $ \mov -> do
          cur <- get
          b <- prePruning cur mov
          if b
            then do
            withStep mov $ do
              next <- get
              c <- pruning cur mov next
              if c
                then (mov, ) <$> eval undefined hmr 0 (yomiDepth * 10)
                else return (mov, minf)
            else return (mov, minf)
        when (Option.verbose opt) $ do
          liftIO $ print rs
          liftIO $ print . HM.size =<< readIORef hmr
          liftIO $ print . sum . map length . map snd . HM.toList =<< readIORef hmr
        return $ maximumBy (comparing snd) rs


      (w,h) <- getSize
      roboPos <- access llPosL
      let radius :: Double
          radius = fromIntegral $ w+h
      bd <- access llBoardL

      greedyDepth <- liftIO $ Oracle.ask oracle "greedyDepth" $ return 4
      valueField <-
        if step > 0
          then liftIO $ readIORef valueFieldRef
          else do
            ret <- newF (0::Double)
            liftIO $ writeIORef valueFieldRef ret
            return ret
      loveField <-
        if step > 0
          then liftIO $ readIORef loveFieldRef
          else do
            newLoveField <- newF (0::Double)
            forPos $ \ r -> do
              a <- unsafeReadF bd r
              when (a `elem` "ABCDEFGHI!" && itemLoveAmp /= 0) $ do
                printe "ATTAGIGE"
                writeF newLoveField r =<< liftIO (randomRIO (0, itemLoveAmp * radius))
              when (placeLoveNum > 0) $ do
                dice <- liftIO $ randomRIO (0,1)
                when (dice < placeLoveNum /(fromIntegral $ w*h)) $ do
                  writeF newLoveField r =<< liftIO (randomRIO (0, placeLoveAmp * radius))
            liftIO $ writeIORef loveFieldRef newLoveField
            return newLoveField
      -- love disappears when you reach there...
      writeF loveField roboPos 0

      razors <- access llRazorsL
      let passable
            | razors > 0 = " .!\\RWA"
            | otherwise  = " .!\\RA"
      dijkstraEX valueField "\\O" passable motionWeight earthDrug loveField 0
      val <- unsafeReadF valueField roboPos
      (mov2, yabasa) <-  do
               cand <- forM "LRUD" $ \hand -> do
                     val3 <- unsafeReadF valueField $ roboPos + hand2pos hand
                     flag <- badnessCheck history hyperHistory greedyDepth hand
                     return ((flag,val3), hand)
               let top = head $ sort $ cand
               return $ (snd top {-move-}, fst (fst top) {-whether it was safe-})

      combineBFFirst <- liftIO $ Oracle.ask oracle "combineBFFirst" $ return True
      perfectGreedy <- liftIO $ Oracle.ask oracle "perfectGreedy" $ return $ False
      perfectSearch <- liftIO $ Oracle.ask oracle "perfectSearch" $ return $ False
      movRand <- liftIO $ choose "RLDU"
      let mov3
           | kabutta                                    = 'A'
           | perfectSearch                              = mov
           | perfectGreedy                              = mov2
           | combineBFFirst && (mov /= 'A' || val <= 0) = mov
           | combineBFFirst                             = mov2
           | not (combineBFFirst) && (yabasa > 0)       = mov2
           | otherwise                                  = mov

      when (Option.verbose opt) $ liftIO $ putStrLn $ "score : " ++ show sc ++ ", move: " ++  [mov,mov2]
      liftIO $ sleep 100
      return $ Ans.Cont mov3

ordering :: LL [Char]
ordering = do
  LLState {..} <- get
  pref <- forM (zip adj4m adj4) $ \(mv, d) -> do
    let pp = llPos + d
    cell <- liftIO $ readPos llBoard pp
    let lev =
          case cell of
            '\\' -> 20
            '!' -> 15
            '*' -> 10
            '@' -> 10
            _   -> 5
    return (lev, mv)

  return $ map snd . reverse . sort $ pref ++ zip [2, 1, 0] "SWA"


prePruning :: LLState -> Char -> LL Bool
prePruning LLState {..} move
  | move == 'S' && llRazors == 0 =
    return False
  | otherwise = do
    return True

pruning :: LLState -> Char -> LLState -> LL Bool
pruning cur move next
  | move `elem` "LRUD" && llPos cur == llPos next =
    return False
  | otherwise = do
    return True

scoreOfs = 10^4

staticScore :: Field Int -> LL Int
staticScore _ = do
  LLState {..} <- get
  abt <- abortScore
  return $ (abt * scoreOfs) - llPenalty

eval :: Field Int -> Cache -> Int -> Int -> LL Int
eval valueField cache !curBest !fuel = do
  mb <- score
  case mb of
    Just sc -> return $ sc * scoreOfs
    _ | fuel <= 0 -> staticScore valueField
    _ -> do
      addCacheEntry cache $ do
        best cache moves $ \cost -> do
          -- liftIO $ putStrLn $ "fuel: " ++ show fuel ++ ", mov: " ++ [mov]
          -- showStatus
          eval valueField cache curBest (fuel - cost)

best :: Cache -> [Char] -> (Int -> LL Int) -> LL Int
best cache ls m = do
  ols <- ordering
  res <- forM ls $ \x -> do
    cur <- get

    emps <- liftIO $ forM adj4 $ \d -> do
      cell <- readPos (llBoard cur) (llPos cur + d)
      return $ if cell `elem` "#*@W123456789" then 0 else 1
    let cost
          | sum emps <= 2 = 3
          | sum emps == 3 = 8
          | otherwise = 10

    pp <- prePruning cur x
    if pp
      then do
      withStep x $ do
        next <- get
        p <- pruning cur x next
        if p then m cost else return minf
      else return minf
  return $ maximum res
