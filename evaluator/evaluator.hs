{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where
import Data.Conduit
import qualified Data.Conduit.Filesystem as FC
import Filesystem
import Filesystem.Path.CurrentOS hiding (null)
import Data.Monoid
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Writer
import Prelude hiding (FilePath, readFile, head)
import System.Environment
import Control.Applicative
import Data.List (sortBy)
import Data.Ord
import Data.Function
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (name, title, style)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import qualified Data.Set as S
import Data.Maybe

data Result
  = Win
  | Abort
  | Dead
  | Cont
  deriving (Show, Eq, Ord,Read)

type Hash = BS.ByteString
type Problem = BS.ByteString
type Strategy = M.Map String String
type ScoreTable = M.Map (Hash, Problem) (Int, Result)
data Env = Env { scores :: ScoreTable
               , strategies :: M.Map Hash Strategy
               , hashes :: S.Set Hash
               , problems :: S.Set Problem
               } deriving (Show, Eq, Ord)
type Machine = WriterT Env (ResourceT IO)

instance Monoid Env where
  mempty  = Env mempty mempty mempty mempty
  mappend (Env a b c d) (Env a' b' c' d') =
      Env (a <> a') (b <> b') (c <> c') (d <> d')

runMachine :: Machine () -> IO Env
runMachine m = runResourceT $ execWriterT m

main :: IO ()
main = do
  dirs <- map decodeString <$> getArgs
  env <- runMachine $ mapM_ (FC.traverse False) dirs $$ mySink
  putStrLn $ renderHtml $ buildHTML env

mySink :: Sink FilePath Machine ()
mySink = awaitForever $ \fp -> do
  isData <- liftIO $ isFile fp
  when isData $ do
    let hash = BS.pack $ encodeString $ dirname fp
    ls <- liftIO $ BS.lines <$> readFile fp
    when (not $ null ls) $ do
      let (scrRsl, prob, strategy) =
            case ls of
              [_, scrRsl, prob, _ , strategy] -> (scrRsl, prob, strategy)
              [scrRsl, prob, _, strategy] -> (scrRsl, prob, strategy)
          (pts:r:_) = words $ BS.unpack scrRsl
          (score, rslt) = (read pts, r)
      lift $ tell $ Env (M.singleton (hash, prob) (score, read rslt))
                        (M.singleton hash $ read $ BS.unpack strategy)
                        (S.singleton hash)
                        (S.singleton prob)
buildHTML :: Env -> Html
buildHTML env@(Env scores strategies hashes probs) =
  docTypeHtml $ do
    head $ do
      style ! type_ "text/css" $ toHtml css
      title "statistics"
    body $ do
      h1 "Oracle Statistics"
      h2 "Rankings"
      renderRanking env
      h2 "Score Status"
      renderScores env
      h2 "Strategy Table"
      renderStrategy strategies

css :: String
css = "td, th{border: solid 1px;}\n.Abort {background-color: yellow;}\n.Win{background-color: #5f5;}\n.Dead{background-color:#f55;}"

renderRanking :: Env -> Html
renderRanking env = do
  renderAverageRanking env
  renderScoreRanking env
  renderWinRanking env

renderAverageRanking Env{scores} = do
  let totals = sortBy (flip $ comparing snd) $ M.toList $ fmap average $ M.mapKeysWith (<>) fst $ fmap (pure . fst) scores
  h3 "average ranking"
  ol $ do
    forM_ (take 10 totals) $ \(h, totl) ->
      li $ do
        mkLink h
        toHtml $ " (" <> show totl <> " pts)"
  

renderWinRanking :: Env -> Html
renderWinRanking Env{scores} = do
  let totals = sortBy (flip $ comparing snd) $ M.toList $ M.mapKeysWith (+) fst $ fmap (const 1) $ M.filter ((==Win).snd) scores
  h3 "win count ranking"
  ol $ do
    forM_ (take 10 totals) $ \(h, totl) ->
      li $ do
        mkLink h
        toHtml $ " (" <> show totl <> "wins)"

renderScoreRanking :: Env -> Html
renderScoreRanking Env {scores, problems, hashes} = do
  let totals = sortBy (flip $ comparing snd) $ M.toList $ M.mapKeysWith (+) fst $ fmap fst scores
  h3 "score ranking"
  ol $ do
    forM_ (take 10 totals) $ \(h, totl) ->
      li $ do
        mkLink h
        toHtml $ " (" <> show totl <> "pts)"

mkLink :: Hash -> Html
mkLink h = a ! href (toValue $  '#':BS.unpack h) $ toHtml (BS.unpack h)

renderScores :: Env -> Html
renderScores Env {scores, problems, hashes} = do
  table $ tr $ do
    td ! class_ "Win" $ "Win"
    td ! class_ "Abort" $ "Abort"
    td ! class_ "Dead" $ "Dead"
  table $ do
    let probs = S.toList problems
    tr $ do
      th ""
      forM_ probs $ \prob -> th $ toHtml (BS.unpack prob)
    forM_ (S.toList hashes) $ \h -> do
      tr $ do
        let avr = average $ map (\p -> maybe 0 fst $ M.lookup (h, p) scores) probs
        th (mkLink h <> br <> "Avr: " <> (toHtml $ show avr))
        forM_ probs $ \p ->
          maybe (td ! class_ "empty" $ "") renderResult $ M.lookup (h, p) scores
    where
    renderResult (pt, sc) = td ! class_ (toValue $ show sc) $ (toHtml $ show pt)

average :: [Int] -> Float
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

renderStrategy :: M.Map Hash Strategy -> Html
renderStrategy strategies = table $ do
  let sts  = S.toList $ mconcat $ map M.keysSet $ M.elems strategies
  tr $ do
    th ""
    forM_ sts $ \stName -> th (toHtml stName)
  forM_ (M.toList strategies) $ \(hash, strat) -> do
    tr $ do
      th ! A.id (toValue $ BS.unpack hash) $ toHtml (BS.unpack hash)
      forM_ sts $ \name -> td $ maybe "" toHtml $ M.lookup name strat
