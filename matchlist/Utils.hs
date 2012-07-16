{-# LANGUAGE DeriveDataTypeable #-}

module Utils
       ( sinkJSON
       , sinkFromJSON
       ) where

import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AT
import Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CA
import Data.Data
import Control.Monad.Trans.Class
import Control.Exception.Lifted as E

data ParserError
  = ParserError String
  deriving (Show, Data, Typeable)
instance Exception ParserError

sinkJSON :: C.MonadResource m => C.Sink ByteString m Value
sinkJSON = CA.sinkParser json

sinkFromJSON :: (FromJSON a, C.MonadResource m) => C.Sink ByteString m a
sinkFromJSON = do
  v <- sinkJSON
  case fromJSON v of
    AT.Error err -> lift $ C.monadThrow $ ParserError err
    AT.Success r -> return r
