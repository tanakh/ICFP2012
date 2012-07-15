{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Conduit
import qualified Data.Conduit.Filesystem as FC
import Filesystem
import Filesystem.Path.CurrentOS
import Data.Monoid
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Writer
import Prelude hiding (FilePath, readFile)
import System.Environment
import Control.Applicative

type Hash = BS.ByteString
type Problem = BS.ByteString
type Strategy = M.Map String String
type ScoreTable = M.Map (Hash, Problem) Int
data Env = Env { scores :: ScoreTable
               , strategies :: M.Map Hash Strategy
               } deriving (Show, Eq, Ord)
type Machine = WriterT Env (ResourceT IO)

instance Monoid Env where
  mempty  = Env mempty mempty
  mappend (Env a b) (Env a' b') = Env (a <> a') (b <> b')

runMachine :: Machine () -> IO Env
runMachine m = runResourceT $ execWriterT m

main :: IO ()
main = do
  dirs <- map decodeString <$> getArgs
  t <- runMachine $ mapM_ (FC.traverse False) dirs $$ mySink
  print t

mySink :: Sink FilePath Machine ()
mySink = awaitForever $ \fp -> do
  isData <- liftIO $ isFile fp
  when isData $ do
    let hash = T.encodeUtf8 $ encode $ dirname fp
    [_, score, prob, _, strategy] <- liftIO $ BS.lines <$>readFile fp
    lift $ tell $ Env (M.singleton (hash, prob) $ read $ BS.unpack score)
                      (M.singleton hash $ read $ BS.unpack strategy)
