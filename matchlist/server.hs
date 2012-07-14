{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

import Web.Scotty hiding (body)
import Text.Hastache
import Text.Hastache.Context
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Data.Aeson as A
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as BL
import Data.Data
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Applicative ((<$>))
import Control.Monad.Trans
import Control.Monad
import System.Directory (getDirectoryContents)
import qualified System.IO as IO
import System.FilePath ((</>))

import AIResult

data Results =
  Results
  { rAIs :: [AISpec]
  , rMapResults :: [EachMap]
  } deriving (Data, Typeable, Show)

data EachMap =
  EachMap
  { eMapSpec :: MapSpec
  , eMapResults :: Text
  } deriving (Data, Typeable, Show)

mustache :: (Data a, Typeable a) => FilePath -> a -> IO Text
mustache path context = do
  TL.decodeUtf8 <$> hastacheFile defaultConfig path (mkGenericContext context)

resultDir :: FilePath
resultDir = "results"

getSolvedResultList :: IO [SolvedResult]
getSolvedResultList = do
  files <- getDirectoryContents resultDir
  let list = (map (resultDir </>) . filter (isSuffixOf ".result")) $ files
  dat <- forM list $ \f -> A.decode <$> BL.readFile f
  return $ catMaybes dat

getResults :: [SolvedResult] -> IO Results
getResults results = do
  tbl <- mapM eachMap maps
  return $ Results { rAIs = ais, rMapResults = tbl }
  where
    catresults = M.fromList
                 $ map (\r -> ((sMapSpec r, sAISpec r), r))
                 $ results
    uniqueBy f lst = M.keys $ M.fromList $ map ((,True) . f) lst
    maps = uniqueBy sMapSpec results
    ais = uniqueBy sAISpec results
    eachMap mSpec = do
      let lst = flip map ais $ \aSpec -> M.lookup (mSpec, aSpec) catresults
      tbl <- renderTable lst
      return $ EachMap mSpec (TL.concat tbl)

renderTable :: [Maybe SolvedResult] -> IO [Text]
renderTable results = forM results $ \r -> do
  case r of
    Just v | isJust (sExitCode v) -> mustache "./views/td.mustache" v
           | otherwise -> mustache "./views/td.lte.mustache" v
    Nothing -> return "<td>N/A</td>"

main :: IO ()
main = do
  --renderResult >>= TL.putStrLn
  serv

renderResult :: IO Text
renderResult = do
  results <- getSolvedResultList >>= getResults
  mustache "./views/home.mustache" results

serv :: IO ()
serv = scotty 3000 $ do
    middleware $ staticPolicy $ addBase "results"

    get "/" $ do
      liftIO renderResult >>= html
