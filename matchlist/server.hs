{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

import Web.Scotty hiding (body)
import Text.Hastache
import Text.Hastache.Context
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Data.Aeson as A
import Data.Text.Lazy.Encoding (decodeUtf8)
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
  } deriving (Data, Typeable)

data EachMap =
  EachMap
  { mapName :: String
  , mapPath :: FilePath
  , mapAIResults :: [Result]
  } deriving (Data, Typeable)

data R = R { existR :: Maybe Result } deriving (Data, Typeable)


mustache :: (Data a, Typeable a) => FilePath -> a -> ActionM ()
mustache path context = do
  body <- liftIO $ decodeUtf8 <$> hastacheFile defaultConfig path (mkGenericContext context)
  html body

-- withRescue :: ActionM () -> ActionM ()
-- -- TODO Return proper status code
-- withRescue = flip rescue text

resultDir :: FilePath
resultDir = "results"

getResultList :: IO [Result]
getResultList = do
  files <- getDirectoryContents resultDir
  let list = (map (resultDir </>) . filter (isSuffixOf ".result")) $ files
  dat <- forM list $ \f -> A.decode <$> BL.readFile f
  return $ catMaybes dat

getResults :: [Result] -> Results
getResults results = Results { rAINames = ainames, rMapResults = mapResults }
  where
    catresults = M.fromList
                 $ map (\r -> ((resultAIPath r, resultMapPath r), r))
                 $ results
    maps = M.keys $ M.fromList $ map ((,True) . resultMapPath) results
    ais = M.keys $ M.fromList $ map ((,True) . resultAIPath) results
    ainames = map takeFileName ais
    eachmap mpath =
      let mname = takeFileName mpath
          lst = flip map ais $ \apath -> M.lookup (apath, mpath) catresults
      in EachMap mname mpath (map fromJust lst)
    mapResults = map eachmap maps

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy $ addBase "results"

    get "/" $ do
      results <- liftIO $ getResults <$> getResultList

      mustache "views/home.mustache" $ results
