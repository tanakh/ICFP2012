import DefaultMain
import Ans
import qualified AI.Oracle as Oracle

main :: IO ()
main = do
  o <- Oracle.new "default"
  defaultMain o $ return End
