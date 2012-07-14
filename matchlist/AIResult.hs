{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module AIResult where

import Data.Aeson.TH
import Data.Data

data Result =
  Result
  { resultExitCode :: Maybe Int
  , resultAIPath :: FilePath
  , resultAIName :: String
  , resultType :: String
  , resultScore :: Int
  , resultBaseName :: String
  , resultMapPath :: FilePath
  , resultTimeElapsed :: Double
  }
  deriving (Show, Data, Typeable)
$(deriveJSON (drop 6) ''Result)
