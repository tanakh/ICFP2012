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
import System.FilePath (takeFileName, (</>))

import AIResult

data Results =
  Results
  { rAINames :: [FilePath]
  , rMapResults :: [EachMap]
  } deriving (Data, Typeable, Show)

data EachMap =
  EachMap
  { mapName :: String
  , mapPath :: FilePath
  , mapResults :: Text
  } deriving (Data, Typeable, Show)

mustache :: (Data a, Typeable a) => FilePath -> a -> IO Text
mustache path context = do
  TL.decodeUtf8 <$> hastacheFile defaultConfig path (mkGenericContext context)

resultDir :: FilePath
resultDir = "results"

getResultList :: IO [Result]
getResultList = do
  files <- getDirectoryContents resultDir
  let list = (map (resultDir </>) . filter (isSuffixOf ".result")) $ files
  dat <- forM list $ \f -> A.decode <$> BL.readFile f
  return $ catMaybes dat

getResults :: [Result] -> IO Results
getResults results = do
  tbl <- mapM eachMap maps
  return $ Results { rAINames = ainames, rMapResults = tbl }
  where
    catresults = M.fromList
                 $ map (\r -> ((resultAIPath r, resultMapPath r), r))
                 $ results
    uniqueBy f lst = M.keys $ M.fromList $ map ((,True) . f) lst
    maps = uniqueBy resultMapPath results
    ais = uniqueBy resultAIPath results
    ainames = map takeFileName ais
    eachMap mpath = do
      let mname = takeFileName mpath
          lst = flip map ais $ \apath -> M.lookup (apath, mpath) catresults
      tbl <- renderTable lst
      return $ EachMap mname mpath (TL.concat tbl)

renderTable :: [Maybe Result] -> IO [Text]
renderTable results = forM results $ \r -> do
  case r of
    Just v -> mustache "views/td.mustache" v
    Nothing -> return "<td>N/A</td>"

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy $ addBase "results"

    get "/" $ do
      results <- liftIO $ getResultList >>= getResults
      body <- liftIO $ mustache "views/home.mustache" results
      html body
